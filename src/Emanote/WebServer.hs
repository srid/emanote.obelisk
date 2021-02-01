{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Emanote.WebServer (run) where

import qualified Data.Map.Strict as Map
import Data.Tagged
import qualified Data.Text as T
import qualified Emanote.Graph as G
import Emanote.Markdown.WikiLink
import qualified Emanote.Markdown.WikiLink as W
import Emanote.Zk (Zk (..))
import qualified Network.Wai.Middleware.Static as MStatic
import Reflex.Dom.Builder.Static (renderStatic)
import qualified Reflex.Dom.Pandoc as PR
import qualified Reflex.TIncremental as TInc
import Text.Mustache (ToMustache, object, (~>))
import qualified Text.Mustache as Mustache
import Text.Pandoc.Definition (Block, Pandoc (Pandoc))
import Web.Scotty (html, param, scotty)
import qualified Web.Scotty as Scotty

run ::
  FilePath ->
  Zk ->
  IO ()
run inputDir Zk {..} = do
  scotty 3000 $ do
    Scotty.middleware $ MStatic.staticPolicy (MStatic.addBase inputDir)
    Scotty.get "/" $ do
      wIds <- reverse . Map.keys <$> TInc.readValue _zk_zettels
      -- TODO: Template? Needs includes first, I think.
      -- Also, need to support search here. So considerate that too when templatifying.
      html $
        toLazy $
          T.unwords $
            wIds <&> \(untag -> x) ->
              "<li><a href=\"" <> x <> "\">" <> x <> "</a></li>"
    Scotty.get "/:wikiLinkID" $ do
      wikiLinkID <- Tagged <$> param "wikiLinkID"
      zs <- Map.lookup wikiLinkID <$> TInc.readValue _zk_zettels
      case zs of
        Nothing -> Scotty.text "404"
        Just v ->
          case v of
            Left err -> Scotty.text $ "Conflict: " <> show err
            Right (_fp, Left err) -> Scotty.text $ "Parse: " <> show err
            Right (_fp, Right doc) -> do
              mIndexTmpl <- Map.lookup "templates/note.html" <$> TInc.readValue _zk_htmlTemplate
              case mIndexTmpl of
                Nothing -> Scotty.text "Write your templates/note.html, dude"
                Just (Left err) -> Scotty.text $ "oopsy template: " <> show err
                Just (Right tmpl) -> do
                  mdHtml <- renderPandoc doc
                  graph <- TInc.readValue _zk_graph
                  let mkLinkCtxList f = do
                        let ls = uncurry mkLinkContext <$> G.connectionsOf f wikiLinkID graph
                        traverse (traverse (renderPandoc . Pandoc mempty)) ls
                  page <-
                    Page wikiLinkID mdHtml
                      <$> mkLinkCtxList (\l -> W.isReverse l && not (W.isParent l)) -- Backlinks (sans uplinks)
                      <*> mkLinkCtxList W.isBranch -- Downlinks
                      <*> mkLinkCtxList W.isParent -- Uplinks
                  html $ toLazy $ Mustache.substitute tmpl page

-- | TODO: Do this in TIncremental for performance
renderPandoc :: MonadIO m => Pandoc -> m Html
renderPandoc =
  fmap (Html . decodeUtf8 . snd) . liftIO . renderStatic . PR.elPandoc PR.defaultConfig

data LinkContext ctx = LinkContext
  { _linkcontext_id :: WikiLinkID,
    _linkcontext_label :: WikiLinkLabel,
    _linkcontext_ctx :: ctx
  }
  deriving (Functor, Foldable, Traversable)

newtype Html = Html {unHtml :: Text}
  deriving (Eq)

instance ToMustache Html where
  toMustache = Mustache.toMustache . unHtml

mkLinkContext :: (WikiLinkLabel, WikiLinkContext) -> WikiLinkID -> LinkContext [Block]
mkLinkContext (_linkcontext_label, _linkcontext_ctx) _linkcontext_id = do
  LinkContext {..}

data Page = Page
  { _page_wikiLinkID :: WikiLinkID,
    _page_mdHtml :: Html,
    _page_backlinks :: [LinkContext Html],
    _page_downlinks :: [LinkContext Html],
    _page_uplinks :: [LinkContext Html]
  }

instance ToMustache (LinkContext Html) where
  toMustache LinkContext {..} =
    object
      [ "id" ~> untag _linkcontext_id,
        "label" ~> show @Text _linkcontext_label,
        "ctxHtml" ~> _linkcontext_ctx
      ]

instance ToMustache Page where
  toMustache Page {..} =
    object
      [ "wikiLinkID" ~> untag _page_wikiLinkID,
        "mdHtml" ~> _page_mdHtml,
        "backlinks" ~> _page_backlinks,
        "uplinks" ~> _page_uplinks,
        "downlinks" ~> _page_downlinks
      ]
