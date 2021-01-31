{-# LANGUAGE GADTs #-}

module Main where

import Control.Concurrent (forkIO)
import qualified Emanote.Pipeline as Pipeline
import qualified Emanote.WebServer as WS
import GHC.IO.Handle (BufferMode (LineBuffering), hSetBuffering)
import Main.Utf8 (withUtf8)
import Options.Applicative
import Reflex (Reflex (never))
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
    runHeadlessApp $ do
      zk <- Pipeline.run inputDir
      void $
        liftIO $
          forkIO $
            WS.run inputDir zk
      pure never
