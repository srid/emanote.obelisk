module G.WebServer (run) where

import qualified Data.Map.Strict as Map
import Data.Tagged (Tagged (Tagged), untag)
import G.Db (Db (..))
import G.Db.Types.Zk (Zk (..))
import G.Db.Types.Zk.Patch (ZkPatch)
import G.Markdown.WikiLink (WikiLinkID)
import qualified Network.Wai.Middleware.Static as MStatic
import Reflex.Dom.Core
import qualified Reflex.Dom.Pandoc as PR
import qualified Shower
import Text.Pandoc.Definition (Pandoc)
import Web.Scotty (html, param, scotty)
import qualified Web.Scotty as Scotty

run ::
  FilePath ->
  Db ZkPatch ->
  IO ()
run inputDir Db {..} = do
  scotty 3000 $ do
    Scotty.middleware $ MStatic.staticPolicy (MStatic.addBase inputDir)
    Scotty.get "/:wikiLinkID" $ do
      wikiLinkID <- Tagged <$> param "wikiLinkID"
      zk@Zk {..} <- liftIO $ readTVarIO _db_data
      case Map.lookup wikiLinkID _zk_zettels of
        Nothing -> Scotty.text "404"
        Just v ->
          case v of
            Left err -> Scotty.text $ "Conflict: " <> show err
            Right (_fp, Left err) -> Scotty.text $ "Parse: " <> show err
            Right (_fp, Right doc) -> do
              s <-
                fmap snd $
                  liftIO $
                    renderStatic $ vertexWidget zk wikiLinkID doc
              html $ decodeUtf8 s

vertexWidget :: (PR.PandocBuilder t m) => Zk -> WikiLinkID -> Pandoc -> m ()
vertexWidget Zk {..} wlId doc = do
  -- NOTE: Aim to use semantic tags, and let the user style it via custom CSS (not Clay)
  -- TODO: Read https://css-tricks.com/how-to-section-your-html/
  el "html" $ do
    el "head" $ do
      el "title" $ text $ untag wlId
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: "/style.css") blank
    el "body" $ do
      elAttr "div" ("id" =: "container") $ do
        el "main" $ do
          el "header" $ el "h1" $ text "G"
          el "article" $ do
            el "h1" $ text $ untag wlId
            PR.elPandoc PR.defaultConfig doc
          elClass "nav" "backlinks" $ do
            el "h2" $ text "Backlinks"
            text "TODO: backlinks"
        el "footer" $ do
          el "pre" $ text $ toText $ Shower.shower _zk_graph
