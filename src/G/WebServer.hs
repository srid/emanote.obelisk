{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module G.WebServer (run) where

import Control.Lens (makeLenses)
import qualified Data.Map.Strict as Map
import Data.Map.Syntax
import Data.Tagged
import G.Db (Db (..))
import G.Markdown.WikiLink (ID)
import qualified Heist.Interpreted as I
import Reflex.Dom.Builder.Static (renderStatic)
import qualified Reflex.Dom.Pandoc as PR
import qualified Shower
import Snap.Core
import Snap.Http.Server (defaultConfig)
import Snap.Snaplet
import Snap.Snaplet.Heist.Interpreted
import Snap.Util.FileServe (serveDirectory)
import Text.Pandoc.Definition (Pandoc)
import qualified Text.XmlHtml as X

data App = App
  { _app_heist :: Snaplet (Heist App)
  }

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet app_heist

type AppHandler = Handler App App

handleMThing :: Db -> AppHandler ()
handleMThing Db {..} = do
  getParam "thing" >>= \case
    Nothing ->
      writeBS "404"
    Just (Tagged . decodeUtf8 -> (thingID :: ID)) -> do
      m <- liftIO (readTVarIO _db_data)
      case Map.lookup thingID m of
        Nothing ->
          writeBS "Thing not found"
        Just thing ->
          handleThing thing
  where
    handleThing thing = do
      -- TODO: Refactor, and do it properly
      let mkSplice = case thing of
            Left c -> I.textSplice $ toText $ Shower.shower c
            Right (_fp, Left err) -> I.textSplice $ toText $ Shower.shower err
            Right (_fp, Right doc) -> do
              html <- liftIO (renderPandoc doc)
              -- TODO: Don't do this. Just write commonmark renderer, that
              -- directly creates these nodes.
              case X.parseXML "<from-pandoc>" html of
                Left err -> I.textSplice (toText err)
                Right (X.HtmlDocument _ _ nodes) ->
                  pure nodes
                Right (X.XmlDocument _ _ nodes) ->
                  pure nodes
      heistLocal
        (I.bindSplices $ "somethin" ## mkSplice)
        (render "index")
    renderPandoc :: Pandoc -> IO ByteString
    renderPandoc =
      fmap snd . renderStatic . PR.elPandoc PR.defaultConfig

routes :: FilePath -> Db -> [(ByteString, AppHandler ())]
routes outputDir db =
  [ ("/:thing", handleMThing db),
    ("static", serveDirectory outputDir)
  ]

app :: FilePath -> Db -> SnapletInit App App
app outputDir db = makeSnaplet "app" "An snaplet example application." Nothing $ do
  -- TODO: This looks for ./snaplets/heist/templates; look instead in source
  -- directory. Why is Snap hardcoding this path?
  h <- nestSnaplet "/heist_debug" app_heist $ heistInit "templates"
  addRoutes $ routes outputDir db
  pure $ App h

run ::
  FilePath ->
  Db ->
  IO ()
run outputDir db = do
  serveSnaplet defaultConfig (app outputDir db)
