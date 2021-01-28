{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Main where

import qualified Commonmark.Syntax as CM
import Control.Monad.Fix (MonadFix)
import Data.Conflict (Conflict (..))
import qualified Data.Conflict as Conflict
import qualified Data.Conflict.Patch as Conflict
import Data.List (nubBy)
import qualified Data.Map as Map
import Data.Tagged (Tagged (..))
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime)
import qualified G.Markdown as M
import qualified G.Markdown.WikiLink as M
import Options.Applicative
import Reflex
import Reflex.Dom.Builder.Static (renderStatic)
import qualified Reflex.Dom.Pandoc as PR
import Reflex.FSNotify (FSEvent, watchTree)
import Reflex.Host.Headless (runHeadlessApp)
import System.Directory (makeAbsolute, removeFile)
import qualified System.FSNotify as FSN
import System.FilePath (addExtension, addTrailingPathSeparator, dropExtension, isRelative, makeRelative, takeExtension, takeFileName, (</>))
import System.FilePattern (FilePattern)
import qualified System.FilePattern as FP
import qualified System.FilePattern.Directory as SFD
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

main :: IO ()
main = do
  (inputDir, outputDir) <- execParser $ info (cliParser <**> helper) fullDesc
  runHeadlessApp $ do
    fsInc <- getDirectoryFiles [".*/**"] inputDir
    let fsIncFinal =
          fsInc
            & pipeFilterExt ".md"
            & pipeFlattenFsTree (Tagged . T.replace " " "-" . T.toLower . toText . dropExtension . takeFileName)
            & pipeParseMarkdown (M.wikiLinkSpec <> M.markdownSpec)
    let xDiff = updatedIncremental fsIncFinal
    x0 <- sample $ currentIncremental fsIncFinal
    liftIO $ putTextLn $ "INI " <> show (fmap (second fst) x0)
    forM_ (Map.toList . unPatchMap $ patchMapInitialize x0) $ uncurry (handleFinal outputDir)
    performEvent_ $
      ffor xDiff $ \m -> do
        liftIO $ putTextLn $ "EVT " <> show (fmap (second fst) m)
        forM_ (Map.toList . unPatchMap $ m) $ uncurry (handleFinal outputDir)
    pure never
  where
    patchMapInitialize :: Map k v -> PatchMap k v
    patchMapInitialize = PatchMap . fmap Just

handleFinal ::
  MonadIO m =>
  FilePath ->
  ID ->
  Maybe (Either (Conflict FilePath ByteString) (FilePath, Either M.ParserError Pandoc)) ->
  m ()
handleFinal outputDir (Tagged fId) mv = do
  let k = addExtension (toString fId) ".html"
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

pipeDiscardContent :: (Reflex t, Ord k) => Incremental t (PatchMap k v) -> Incremental t (PatchMap k ())
pipeDiscardContent =
  unsafeMapIncremental void void

pipeFilterExt ::
  Reflex t =>
  String ->
  Incremental t (PatchMap FilePath v) ->
  Incremental t (PatchMap FilePath v)
pipeFilterExt ext =
  unsafeMapIncremental
    (Map.mapMaybeWithKey $ \fs x -> guard (takeExtension fs == ext) >> pure x)
    (PatchMap . Map.mapMaybeWithKey (\fs x -> guard (takeExtension fs == ".md") >> pure x) . unPatchMap)

pipeParseMarkdown ::
  (Reflex t, Functor f, Functor g, M.MarkdownSyntaxSpec m il bl) =>
  CM.SyntaxSpec m il bl ->
  Incremental t (PatchMap ID (f (g ByteString))) ->
  Incremental t (PatchMap ID (f (g (Either M.ParserError Pandoc))))
pipeParseMarkdown spec =
  unsafeMapIncremental
    (Map.mapWithKey $ \fId -> (fmap . fmap) (parse fId))
    (PatchMap . Map.mapWithKey ((fmap . fmap . fmap) . parse) . unPatchMap)
  where
    parse :: ID -> ByteString -> Either M.ParserError Pandoc
    parse (Tagged (toString -> fn)) = M.parseMarkdown spec fn . decodeUtf8

-- | ID of a Markdown file
type ID = Tagged "ID" Text

pipeFlattenFsTree ::
  forall t v.
  (Reflex t) =>
  -- | How to flatten the file path.
  (FilePath -> ID) ->
  Incremental t (PatchMap FilePath v) ->
  Incremental t (PatchMap ID (Either (Conflict FilePath v) (FilePath, v)))
pipeFlattenFsTree toKey = do
  unsafeMapIncrementalWithOldValue
    (Conflict.resolveConflicts toKey)
    (Conflict.applyPatch toKey)

-- | Return a reflex Incremental reflecting the selected files in a directory tree.
--
-- The incremental updates as any of the files in the directory change, or get
-- added or removed.
getDirectoryFiles ::
  ( Reflex t,
    MonadHold t m,
    MonadFix m,
    MonadIO m,
    MonadIO (Performable m),
    PostBuild t m,
    TriggerEvent t m,
    PerformEvent t m
  ) =>
  [FilePattern] ->
  FilePath ->
  m (Incremental t (PatchMap FilePath ByteString))
getDirectoryFiles ignores p = do
  fsEvents <- watchDirWithDebounce 0.1 ignores p
  fs0 <- liftIO $ do
    fs <- SFD.getDirectoryFilesIgnore p ["**"] ignores
    fmap Map.fromList $
      forM fs $ \f -> do
        s <- readFileBS (p </> f)
        pure (f, s)
  fsPatches <- performEvent $
    ffor fsEvents $ \evts ->
      -- FIXME: If the file is gone (probably tmp), we should just ignore this event.
      -- But do it in a way to report to the user?
      liftIO $ readFilesAsPatchMap evts
  holdIncremental fs0 fsPatches
  where
    readFilesAsPatchMap :: [FSN.Event] -> IO (PatchMap FilePath ByteString)
    readFilesAsPatchMap =
      fmap (PatchMap . Map.fromList . catMaybes) . traverse go
      where
        go = \case
          FSN.Added fp _time False ->
            Just . (fp,) . Just <$> readFileBS (p </> fp)
          FSN.Modified fp _time False ->
            Just . (fp,) . Just <$> readFileBS (p </> fp)
          FSN.Removed fp _time False ->
            pure $ Just (fp, Nothing)
          _ ->
            pure Nothing

-- | Like `watchDir` but batches file events, and limits to given dir children.
--
-- Returned event is in the form of list, which is guaranteed to not have repeat
-- events (i.e., 2 or more events with the same eventPath). Events' paths are
-- also guaranteed to be relative to base directory (outside targetting symlinks
-- are discarded).
--
-- TODO: Extend this to return a `Incremental`, and put it in
-- Reflex.FSNotify.Incremental.
watchDirWithDebounce ::
  ( PostBuild t m,
    TriggerEvent t m,
    PerformEvent t m,
    MonadIO (Performable m),
    MonadHold t m,
    MonadIO m,
    MonadFix m
  ) =>
  -- | How long to wait for (seconds) before batching events
  NominalDiffTime ->
  -- | File patterns to ignore
  [FilePattern] ->
  -- | Base directory to monitor
  FilePath ->
  m (Event t [FSEvent])
watchDirWithDebounce ms ignores dirPath' = do
  -- Converting this to an absolute path ensures that the use of `makeRelative`
  -- (further below) works as expected.
  dirPath <- liftIO $ makeAbsolute dirPath'
  let cfg = FSN.defaultConfig {FSN.confDebounce = FSN.Debounce ms}
  pb <- getPostBuild
  fsEvtRaw <- watchTree cfg (dirPath <$ pb) (const True)
  let fsEvt = fforMaybe fsEvtRaw $ \fse' -> do
        fse <- mkEventPathRelative dirPath fse'
        let path = FSN.eventPath fse
            shouldIgnore = any (FP.?== path) ignores
        guard $ not shouldIgnore
        pure fse
  -- Batch quickly firing events, discarding all but the last one for each path.
  fmap (nubByKeepLast ((==) `on` FSN.eventPath) . toList)
    <$> batchOccurrences ms fsEvt
  where
    -- Like @Data.List.nubBy@ but keeps the last occurence
    nubByKeepLast :: (a -> a -> Bool) -> [a] -> [a]
    nubByKeepLast f =
      reverse . nubBy f . reverse
    mkEventPathRelative :: FilePath -> FSN.Event -> Maybe FSN.Event
    mkEventPathRelative baseDir = \case
      FSN.Added fp t d ->
        mkR fp <&> \p -> FSN.Added p t d
      FSN.Modified fp t d ->
        mkR fp <&> \p -> FSN.Modified p t d
      FSN.Removed fp t d ->
        mkR fp <&> \p -> FSN.Removed p t d
      FSN.Unknown fp t d ->
        mkR fp <&> \p -> FSN.Unknown p t d
      where
        mkR fp = do
          let rel = makeRelative baseDir fp
          -- Discard sylinks targetting elsewhere
          guard $ isRelative rel
          pure rel

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
