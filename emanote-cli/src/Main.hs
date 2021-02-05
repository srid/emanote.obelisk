{-# LANGUAGE GADTs #-}

module Main where

import qualified Emanote
import GHC.IO.Handle (BufferMode (LineBuffering), hSetBuffering)
import Main.Utf8 (withUtf8)
import Options.Applicative
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
    Emanote.emanoteMain inputDir
