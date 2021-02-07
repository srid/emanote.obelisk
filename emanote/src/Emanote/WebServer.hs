{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Emanote.WebServer (run) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Tagged
import qualified Data.Text as T
import qualified Emanote.Graph as G
import qualified Emanote.Graph.Patch as GP
import Emanote.Markdown.WikiLink
import qualified Emanote.Markdown.WikiLink as W
import Emanote.Zk (Zk (..))
import qualified Network.Wai.Middleware.Static as MStatic
import Reflex.Dom.Core
import qualified Reflex.Dom.Pandoc as PR
import qualified Reflex.TIncremental as TInc
import Relude
import Text.Mustache (ToMustache, object, (~>))
import qualified Text.Mustache as Mustache
import Text.Pandoc.Definition (Pandoc (Pandoc))
import Web.Scotty (html, param, scotty)
import qualified Web.Scotty as Scotty

run ::
  FilePath ->
  Zk ->
  IO ()
run inputDir Zk {..} = do
  n0 <- Map.size <$> TInc.readValue _zk_zettels
  putTextLn $ toText inputDir <> ": " <> show n0 <> " notes (for now)"
  scotty 3000 $ do
    Scotty.middleware $ MStatic.staticPolicy (MStatic.addBase inputDir)
    Scotty.get "/" $ do
      wIds <- reverse . Map.keys <$> TInc.readValue _zk_zettels
      -- TODO: Template? Needs includes first, I think.
      -- Also, need to support search here. So considerate that too when templatifying.
      html $
        toLazy $
          T.unwords $
            wIds <&> \wId ->
              "<li><a href=\"" <> W.renderWikiLinkUrl wId <> "\">" <> untag wId <> "</a></li>"
    Scotty.get "/:wikiLinkID" $ do
      wikiLinkID <- Tagged <$> param "wikiLinkID"
      zs <- TInc.readValue _zk_zettels
      let errW s = do
            elAttr "div" ("style" =: "border: 1px ridge red; padding: 1em;") $ do
              el "h2" $ text "Oops"
              el "p" $ el "tt" $ text s
      noteHtml <- case Map.lookup wikiLinkID zs of
        Nothing ->
          renderReflexDom $ el "em" $ text "Nothing to display (no Pandoc AST available for this note)."
        Just v ->
          case v of
            Left err ->
              renderReflexDom $ errW $ "Conflict: " <> show err
            Right (_fp, Left err) ->
              renderReflexDom $ errW $ "Parse: " <> show err
            Right (_fp, Right doc) -> do
              renderPandoc doc
      graph <- TInc.readValue _zk_graph
      let mkLinkCtxList f = do
            let ls = uncurry mkLinkContext <$> G.connectionsOf f wikiLinkID graph
            -- Sort in reverse order so that daily notes (calendar) are pushed down.
            sortOn Down <$> traverse (traverse (renderPandoc . Pandoc mempty)) ls
          wikiLinkUrl = W.renderWikiLinkUrl wikiLinkID
          orphans =
            let getVertices = GP.patchedGraphVertexSet (isJust . flip Map.lookup zs) . G.unGraph
                indexed = getVertices $ G.filterBy (\l -> W.isBranch l || W.isParent l) graph
                all' = getVertices graph
                unindexed = all' `Set.difference` indexed
             in Set.toList unindexed <&> mkLinkContext (WikiLinkLabel_Unlabelled, mempty)
      page <-
        Page wikiLinkID wikiLinkUrl noteHtml
          -- TODO: Refactor using enum/dmap/gadt
          <$> mkLinkCtxList (\l -> W.isReverse l && not (W.isParent l) && not (W.isBranch l)) -- Backlinks (sans uplinks / downlinks)
          <*> mkLinkCtxList W.isBranch -- Downlinks
          <*> mkLinkCtxList W.isParent -- Uplinks
          <*> pure orphans
      mIndexTmpl <- Map.lookup "templates/note.html" <$> TInc.readValue _zk_htmlTemplate
      case mIndexTmpl of
        Nothing -> Scotty.text "Write your templates/note.html, dude"
        Just (Left err) -> Scotty.text $ "oopsy template: " <> show err
        Just (Right tmpl) -> do
          html $ toLazy $ Mustache.substitute tmpl page

-- | TODO: Do this in TIncremental for performance
renderPandoc :: MonadIO m => Pandoc -> m Html
renderPandoc =
  renderReflexDom . PR.elPandoc PR.defaultConfig

renderReflexDom :: (MonadIO m) => StaticWidget x a -> m Html
renderReflexDom =
  fmap (Html . decodeUtf8 . snd) . liftIO . renderStatic

data LinkContext ctx = LinkContext
  { _linkcontext_id :: WikiLinkID,
    _linkcontext_url :: Text,
    _linkcontext_label :: WikiLinkLabel,
    _linkcontext_ctx :: ctx
  }
  deriving (Functor, Foldable, Traversable, Eq)

instance Eq (LinkContext ctx) => Ord (LinkContext ctx) where
  compare = compare `on` _linkcontext_id

newtype Html = Html {unHtml :: Text}
  deriving (Eq)

instance ToMustache Html where
  toMustache = Mustache.toMustache . unHtml

mkLinkContext :: (WikiLinkLabel, ctx) -> WikiLinkID -> LinkContext ctx
mkLinkContext (_linkcontext_label, _linkcontext_ctx) _linkcontext_id =
  let _linkcontext_url = W.renderWikiLinkUrl _linkcontext_id
   in LinkContext {..}

data Page = Page
  { _page_wikiLinkID :: WikiLinkID,
    _page_wikiLinkUrl :: Text,
    _page_mdHtml :: Html,
    _page_backlinks :: [LinkContext Html],
    _page_downlinks :: [LinkContext Html],
    _page_uplinks :: [LinkContext Html],
    -- TODO: Use Link Type, as there is no context
    _page_orphans :: [LinkContext ()]
  }

instance ToMustache (LinkContext Html) where
  toMustache LinkContext {..} =
    object
      [ "id" ~> untag _linkcontext_id,
        "url" ~> _linkcontext_url,
        "label" ~> show @Text _linkcontext_label,
        "ctxHtml" ~> _linkcontext_ctx
      ]

instance ToMustache (LinkContext ()) where
  toMustache LinkContext {..} =
    object
      [ "id" ~> untag _linkcontext_id,
        "url" ~> _linkcontext_url,
        "label" ~> show @Text _linkcontext_label
      ]

instance ToMustache Page where
  toMustache Page {..} =
    object
      [ "wikiLinkID" ~> untag _page_wikiLinkID,
        "wikiLinkUrl" ~> _page_wikiLinkUrl,
        "mdHtml" ~> _page_mdHtml,
        "backlinks" ~> _page_backlinks,
        "uplinks" ~> _page_uplinks,
        "downlinks" ~> _page_downlinks,
        "orphans" ~> _page_orphans
      ]
