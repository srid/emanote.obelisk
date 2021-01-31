module G.WebServer (run) where

import qualified Data.Map.Strict as Map
import Data.Tagged (Tagged (Tagged), untag)
import G.Db (Db (..))
import G.Db.Types.Zk (Zk (..))
import G.Db.Types.Zk.Patch (ZkPatch)
import G.Markdown.WikiLink (WikiLinkID)
import qualified Network.Wai.Middleware.Static as MStatic
import Reflex.Dom.Builder.Static (renderStatic)
import qualified Reflex.Dom.Pandoc as PR
import System.FilePath ((</>))
import Text.Mustache (ToMustache, object, (~>))
import qualified Text.Mustache as Mustache
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
      html "Start here: <a href=/index>index</a>"
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
              -- TODO: Move this to Zk{..}, and re compile only when the template changes (Incremental)!
              tmplBody <- liftIO $ readFileText $ inputDir </> "index.html"
              case Mustache.compileTemplate "index.html" tmplBody of
                Left err -> Scotty.text $ "oopsy template: " <> show err
                Right tmpl -> do
                  mdHtml <- fmap snd $ liftIO $ renderStatic $ PR.elPandoc PR.defaultConfig doc
                  let s = Mustache.substitute tmpl (Page wikiLinkID mdHtml)
                  html $ toLazy s

data Page = Page
  { _page_wikiLinkID :: WikiLinkID,
    _page_mdHtml :: ByteString
  }

instance ToMustache Page where
  toMustache Page {..} =
    object
      [ "wikiLinkID" ~> untag _page_wikiLinkID,
        "mdHtml" ~> decodeUtf8 @Text _page_mdHtml
      ]
