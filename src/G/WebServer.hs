{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module G.WebServer where

import G.Db (Db (..))
import Snap.Core
import Snap.Http.Server (quickHttpServe)
import Snap.Util.FileServe (serveDirectory)

run ::
  FilePath ->
  Db ->
  IO ()
run outputDir db = do
  quickHttpServe $ site outputDir db

site :: FilePath -> Db -> Snap ()
site dirPath Db {..} =
  ifTop (writeBS . show . fmap void =<< liftIO (readTVarIO _db_data))
    <|> route
      [ ("k", writeBS "bar"),
        ("echo/:echoparam", echoHandler)
      ]
    <|> dir "static" (serveDirectory dirPath)

echoHandler :: Snap ()
echoHandler = do
  param <- getParam "echoparam"
  maybe
    (writeBS "must specify echo/param in URL")
    writeBS
    param
