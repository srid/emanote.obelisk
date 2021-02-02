module Emanote.FileSystem
  ( directoryTreeIncremental,
    PathContent (..),
  )
where

import Control.Monad.Fix (MonadFix)
import Data.List (nubBy)
import qualified Data.Map as Map
import Data.Time.Clock (NominalDiffTime)
import Reflex
import Reflex.FSNotify (FSEvent, watchTree)
import System.Directory (makeAbsolute)
import qualified System.FSNotify as FSN
import System.FilePath (isRelative, makeRelative, (</>))
import System.FilePattern (FilePattern)
import qualified System.FilePattern as FP
import qualified System.FilePattern.Directory as SFD

data PathContent
  = PathContent_Dir
  | PathContent_File ByteString
  deriving (Eq, Show)

mkPathContent :: Bool -> FilePath -> IO PathContent
mkPathContent isDir fp = do
  if isDir
    then pure PathContent_Dir
    else PathContent_File <$> readFileBS fp

-- | Return a reflex Incremental reflecting the selected files in a directory tree.
--
-- The incremental updates as any of the files in the directory change, or get
-- added or removed.
directoryTreeIncremental ::
  ( Reflex t,
    MonadHold t m,
    MonadFix m,
    MonadIO m,
    MonadIO (Performable m),
    PostBuild t m,
    TriggerEvent t m,
    PerformEvent t m
  ) =>
  -- | File patterns to ignore.
  [FilePattern] ->
  -- | Directory root path.
  FilePath ->
  -- | A reflex @Incremental@ mapping relative path to the file's content.
  m (Incremental t (PatchMap FilePath PathContent))
directoryTreeIncremental ignores p = do
  fsEvents <- watchDirWithDebounce 0.1 ignores p
  fs0 <- liftIO $ do
    fs <- SFD.getDirectoryFilesIgnore p ["**"] ignores
    fmap Map.fromList $
      forM fs $ \f -> do
        -- TODO: we should insert back the directories as well.
        s <- mkPathContent False (p </> f)
        pure (f, s)
  fsPatches <- performEvent $
    ffor fsEvents $ \evts ->
      -- FIXME: If the file is quickly deleted (since event; probably tmp), we
      -- should just ignore this event. But do it in a way to report to the
      -- user?
      -- TODO(!!) Dir updates (added/deleted) don't include their contents!!
      -- In that case, do a mapIncrementalWithOldValue to automatically remove
      -- the file contents.
      liftIO $ readFilesAsPatchMap evts
  holdIncremental fs0 $ traceEvent "pp" fsPatches
  where
    readFilesAsPatchMap :: [FSN.Event] -> IO (PatchMap FilePath PathContent)
    readFilesAsPatchMap =
      fmap (PatchMap . Map.fromList . catMaybes) . traverse go
      where
        go = \case
          FSN.Added fp _time isDir ->
            Just . (fp,) . Just <$> mkPathContent isDir (p </> fp)
          FSN.Modified fp _time isDir ->
            Just . (fp,) . Just <$> mkPathContent isDir (p </> fp)
          FSN.Removed fp _time _isDir ->
            pure $ Just (fp, Nothing)
          _ -> do
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
