{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import qualified Commonmark.Syntax as CM
import Control.Concurrent.Async (race_)
import Data.Conflict (Conflict (..))
import qualified Data.Conflict as Conflict
import qualified Data.Conflict.Patch as Conflict
import qualified Data.Map as Map
import Data.Tagged (Tagged (..))
import qualified Data.Text as T
import qualified G.Db as Db
import G.FileSystem (directoryTreeIncremental)
import qualified G.Markdown as M
import qualified G.Markdown.WikiLink as M
import qualified G.WebServer as WS
import GHC.IO.Handle (BufferMode (LineBuffering), hSetBuffering)
import Main.Utf8 (withUtf8)
import Options.Applicative
import Reflex
import Reflex.Dom.Builder.Static (renderStatic)
import qualified Reflex.Dom.Pandoc as PR
import Reflex.Host.Headless (MonadHeadlessApp, runHeadlessApp)
import System.Directory (removeFile)
import System.FilePath (addExtension, addTrailingPathSeparator, dropExtension, takeExtension, takeFileName)
import Text.Pandoc.Definition (Pandoc)

cliParser :: Parser (FilePath, FilePath)
cliParser =
  (,)
    <$> fmap
      addTrailingPathSeparator
      (strArgument (metavar "INPUT" <> help "Input directory path (.md files)"))
    <*> fmap
      addTrailingPathSeparator
      (strArgument (metavar "OUTPUT" <> help "Output directory path (must exist)"))

type ResultPatches = PatchMap M.ID (Either (Conflict FilePath ByteString) (FilePath, Either M.ParserError Pandoc))

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  withUtf8 $ do
    (inputDir, outputDir) <- execParser $ info (cliParser <**> helper) fullDesc
    db <- Db.newDb
    race_
      (runHeadlessApp $ pipeline inputDir outputDir db)
      ( race_
          (WS.run outputDir db)
          (Db.run db)
      )

pipeline :: MonadHeadlessApp t m => FilePath -> FilePath -> Db.Db -> m (Event t ())
pipeline inputDir outputDir db = do
  input <- directoryTreeIncremental [".*/**"] inputDir
  let output = runPipe input
      drains =
        [ -- Generate static HTML
          mapM_ (uncurry $ generateHtmlFiles outputDir) . Map.toList . unPatchMap,
          -- Communicate the patch to the other thread
          Db.push db
        ]
  drainPipe drains output
  pure never

drainPipe ::
  forall t m k v.
  ( Reflex t,
    PerformEvent t m,
    MonadSample t m,
    MonadIO m,
    MonadIO (Performable m),
    Ord k,
    Show k
  ) =>
  -- | Functions that do the draining of the Incremental.
  --
  -- The initial value will be sent as a @PatchMap@ of only additions.
  [PatchMap k v -> IO ()] ->
  -- | The @Incremental@ coming out at the end of the pipeline.
  Incremental t (PatchMap k v) ->
  m ()
drainPipe fs inc = do
  -- Process initial data.
  x0 <- sample $ currentIncremental inc
  liftIO $ do
    putTextLn $ "INI " <> show (void x0)
    sequence_ $ fs ?? patchMapInitialize x0
  -- Process patches.
  let xE = updatedIncremental inc
  performEvent_ $
    ffor xE $ \m -> do
      liftIO $ do
        putTextLn $ "EVT " <> show (void m)
        sequence_ $ fs ?? m
  where
    -- We "cheat" by treating the initial map as a patch map, that "adds" all
    -- initial data.
    patchMapInitialize :: Map k v -> PatchMap k v
    patchMapInitialize = PatchMap . fmap Just

-- | Pipe the filesystem three through until determining the "final" data.
runPipe ::
  Reflex t =>
  Incremental t (PatchMap FilePath ByteString) ->
  Incremental t (PatchMap M.ID (Either (Conflict FilePath ByteString) (FilePath, Either M.ParserError Pandoc)))
runPipe x =
  x
    & pipeFilterExt ".md"
    & pipeFlattenFsTree (Tagged . T.replace " " "-" . T.toLower . toText . dropExtension . takeFileName)
    & pipeParseMarkdown (M.wikiLinkSpec <> M.markdownSpec)

generateHtmlFiles ::
  MonadIO m =>
  FilePath ->
  M.ID ->
  Maybe (Either (Conflict FilePath ByteString) (FilePath, Either M.ParserError Pandoc)) ->
  m ()
generateHtmlFiles outputDir (Tagged fID) mv = do
  let k = addExtension (toString fID) ".html"
      g = outputDir <> k
  case mv of
    Just (Left conflict) -> do
      liftIO $ putTextLn $ "CON " <> show conflict
      writeFileBS g $ "<p style='color: red'>" <> show conflict <> "</p>"
    Just (Right (_fp, eres)) -> do
      case eres of
        Left (Tagged err) -> do
          liftIO $ putTextLn $ "ERR " <> err
          writeFileText g $ "<pre>" <> err <> "</pre>"
        Right doc -> do
          liftIO $ putTextLn $ "WRI " <> toText k
          s <- liftIO $ renderPandoc doc
          writeFileBS g $ "<pre>" <> s <> "</pre>"
    Nothing -> do
      liftIO $ putTextLn $ "DEL " <> toText k
      liftIO $ removeFile g
  where
    renderPandoc :: Pandoc -> IO ByteString
    renderPandoc =
      fmap snd . renderStatic . PR.elPandoc PR.defaultConfig

pipeFilterExt ::
  Reflex t =>
  String ->
  Incremental t (PatchMap FilePath v) ->
  Incremental t (PatchMap FilePath v)
pipeFilterExt ext =
  let f :: FilePath -> v -> Maybe v
      f = \fs x -> guard (takeExtension fs == ext) >> pure x
   in unsafeMapIncremental
        (Map.mapMaybeWithKey f)
        (PatchMap . Map.mapMaybeWithKey f . unPatchMap)

pipeParseMarkdown ::
  (Reflex t, Functor f, Functor g, M.MarkdownSyntaxSpec m il bl) =>
  CM.SyntaxSpec m il bl ->
  Incremental t (PatchMap M.ID (f (g ByteString))) ->
  Incremental t (PatchMap M.ID (f (g (Either M.ParserError Pandoc))))
pipeParseMarkdown spec =
  unsafeMapIncremental
    (Map.mapWithKey $ \fID -> (fmap . fmap) (parse fID))
    (PatchMap . Map.mapWithKey ((fmap . fmap . fmap) . parse) . unPatchMap)
  where
    parse :: M.ID -> ByteString -> Either M.ParserError Pandoc
    parse (Tagged (toString -> fn)) = M.parseMarkdown spec fn . decodeUtf8

pipeFlattenFsTree ::
  forall t v.
  (Reflex t) =>
  -- | How to flatten the file path.
  (FilePath -> M.ID) ->
  Incremental t (PatchMap FilePath v) ->
  Incremental t (PatchMap M.ID (Either (Conflict FilePath v) (FilePath, v)))
pipeFlattenFsTree toKey = do
  unsafeMapIncrementalWithOldValue
    (Conflict.resolveConflicts toKey)
    (Conflict.applyPatch toKey)

-- | Like `unsafeMapIncremental` but the patch function also takes the old
-- target.
unsafeMapIncrementalWithOldValue ::
  (Reflex t, Patch p, Patch p') =>
  (PatchTarget p -> PatchTarget p') ->
  (PatchTarget p -> p -> p') ->
  Incremental t p ->
  Incremental t p'
unsafeMapIncrementalWithOldValue f g x =
  let x0 = currentIncremental x
      xE = updatedIncremental x
   in unsafeBuildIncremental (f <$> sample x0) $ uncurry g <$> attach x0 xE
