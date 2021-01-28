{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module G.WebServer (run) where

import Control.Lens
import G.Db (Db (..))
import Snap.Core
import Snap.Http.Server (defaultConfig)
import Snap.Snaplet
import Snap.Snaplet.Heist.Interpreted
import Snap.Util.FileServe (serveDirectory)

data App = App
  { _app_heist :: Snaplet (Heist App)
  }

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet app_heist

type AppHandler = Handler App App

handleSomething :: Db -> AppHandler ()
handleSomething Db {..} = do
  x <- liftIO (readTVarIO _db_data)
  writeBS $ show $ fmap void x

routes :: FilePath -> Db -> [(ByteString, AppHandler ())]
routes outputDir db =
  [ ("/something", handleSomething db),
    ("static", serveDirectory outputDir)
  ]

app :: FilePath -> Db -> SnapletInit App App
app outputDir db = makeSnaplet "app" "An snaplet example application." Nothing $ do
  h <- nestSnaplet "" app_heist $ heistInit "templates"
  addRoutes $ routes outputDir db
  pure $ App h

run ::
  FilePath ->
  Db ->
  IO ()
run outputDir db = do
  serveSnaplet defaultConfig (app outputDir db)
