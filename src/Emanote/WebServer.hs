module Emanote.WebServer (run) where

import qualified Data.Map.Strict as Map
import Data.Tagged (Tagged (Tagged), untag)
import qualified Data.Text as T
import Emanote.Db (Db (..))
import Emanote.Db.Types.Zk (Zk (..))
import Emanote.Db.Types.Zk.Patch (ZkPatch)
import qualified Emanote.Graph as G
import Emanote.Markdown.WikiLink (WikiLinkContext, WikiLinkID, WikiLinkLabel)
import qualified Network.Wai.Middleware.Static as MStatic
import Reflex.Dom.Builder.Static (renderStatic)
import qualified Reflex.Dom.Pandoc as PR
import Text.Mustache (ToMustache, object, (~>))
import qualified Text.Mustache as Mustache
import Text.Pandoc.Definition (Pandoc (Pandoc))
import Web.Scotty (html, param, scotty)
import qualified Web.Scotty as Scotty

run ::
  FilePath ->
  Db ZkPatch ->
  IO ()
run inputDir Db {..} = do
  scotty 3000 $ do
    Scotty.middleware $ MStatic.staticPolicy (MStatic.addBase inputDir)
    Scotty.get "/" $ do
      Zk {..} <- liftIO $ readTVarIO _db_data
      let wIds = reverse $ Map.keys _zk_zettels
      -- TODO: Template? Needs includes first, I think.
      -- Also, need to support search here. So considerate that too when templatifying.
      html $
        toLazy $
          T.unwords $
            wIds <&> \(untag -> x) ->
              "<li><a href=\"" <> x <> "\">" <> x <> "</a></li>"
    Scotty.get "/:wikiLinkID" $ do
      wikiLinkID <- Tagged <$> param "wikiLinkID"
      Zk {..} <- liftIO $ readTVarIO _db_data
      case Map.lookup wikiLinkID _zk_zettels of
        Nothing -> Scotty.text "404"
        Just v ->
          case v of
            Left err -> Scotty.text $ "Conflict: " <> show err
            Right (_fp, Left err) -> Scotty.text $ "Parse: " <> show err
            Right (_fp, Right doc) -> do
              case Map.lookup "index.html" _zk_htmlTemplate of
                Nothing -> Scotty.text "Write your index.html, dude"
                Just (Left err) -> Scotty.text $ "oopsy template: " <> show err
                Just (Right tmpl) -> do
                  mdHtml <- fmap snd $ liftIO $ renderStatic $ PR.elPandoc PR.defaultConfig doc
                  backlinks <-
                    liftIO $
                      fmap concat $
                        forM (G.preSetWithLabel wikiLinkID (G.unGraph _zk_graph)) $ \(es, t) ->
                          forM es $ \e ->
                            mkBacklink e t
                  let page = Page wikiLinkID mdHtml backlinks
                      s = Mustache.substitute tmpl page
                  html $ toLazy s

data Backlink = Backlink
  { _backlink_id :: WikiLinkID,
    _backlink_label :: WikiLinkLabel,
    _backlink_ctxHtml :: Text
  }

mkBacklink :: (WikiLinkLabel, WikiLinkContext) -> WikiLinkID -> IO Backlink
mkBacklink (_backlink_label, ctx) _backlink_id = do
  -- TODO: Create backlinks as Incremental, to avoid rendering on demand.
  _backlink_ctxHtml :: Text <-
    fmap (decodeUtf8 . snd) $ liftIO $ renderStatic $ PR.elPandoc PR.defaultConfig $ Pandoc mempty ctx
  pure $ Backlink {..}

data Page = Page
  { _page_wikiLinkID :: WikiLinkID,
    _page_mdHtml :: ByteString,
    _page_backlinks :: [Backlink]
  }

instance ToMustache Backlink where
  toMustache Backlink {..} =
    object
      [ "id" ~> untag _backlink_id,
        "label" ~> show @Text _backlink_label,
        "ctxHtml" ~> _backlink_ctxHtml
      ]

instance ToMustache Page where
  toMustache Page {..} =
    object
      [ "wikiLinkID" ~> untag _page_wikiLinkID,
        "mdHtml" ~> decodeUtf8 @Text _page_mdHtml,
        "backlinks" ~> _page_backlinks
      ]
