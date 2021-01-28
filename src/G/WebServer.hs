module G.WebServer where

import Snap.Core
import Snap.Http.Server (quickHttpServe)
import Snap.Util.FileServe (serveDirectory)

run :: FilePath -> IO ()
run = quickHttpServe . site

site :: FilePath -> Snap ()
site dirPath =
  ifTop (writeBS "hello world")
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
