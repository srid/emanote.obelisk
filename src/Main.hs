{-# LANGUAGE GADTs #-}

module Main where

import Control.Concurrent.Async (race_)
import qualified G.Db as Db
import qualified G.Pipeline as Pipeline
import qualified G.WebServer as WS
import GHC.IO.Handle (BufferMode (LineBuffering), hSetBuffering)
import Main.Utf8 (withUtf8)
import Options.Applicative
import Reflex.Host.Headless (runHeadlessApp)
import System.FilePath (addTrailingPathSeparator)

cliParser :: Parser FilePath
cliParser =
  fmap
    addTrailingPathSeparator
    (strArgument (metavar "INPUT" <> help "Input directory path (.md files)"))

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  withUtf8 $ do
    inputDir <- execParser $ info (cliParser <**> helper) fullDesc
    db <- Db.newDb
    race_
      (runHeadlessApp $ Pipeline.run inputDir db)
      ( race_
          (WS.run db)
          (Db.run db)
      )
