module Main where

import Control.Monad.Fix (MonadFix)
import Data.Conflict (Conflict (..))
import qualified Data.Conflict as Conflict
import qualified Data.Conflict.Patch as Conflict
import Data.List (nubBy)
import qualified Data.Map as Map
import Data.Time.Clock (NominalDiffTime)
import Reflex
import Reflex.FSNotify (FSEvent, watchTree)
import Reflex.Host.Headless (runHeadlessApp)
import System.Directory (makeAbsolute, removeFile)
import qualified System.FSNotify as FSN
import System.FilePath (isRelative, makeRelative, replaceExtension, takeExtension, takeFileName)
import System.FilePattern (FilePattern)
import qualified System.FilePattern as FP
import qualified System.FilePattern.Directory as SFD

main :: IO ()
main = do
  runHeadlessApp $ do
    fsInc <- getDirectoryFiles [".*/**"] "."
    let fsIncFinal =
          fsInc
            & pipeFilterExt ".md"
            & pipeFlattenFsTree (toText . takeFileName)
    let xDiff = updatedIncremental fsIncFinal
    x0 <- sample $ currentIncremental fsIncFinal
    liftIO $ putTextLn $ "INI " <> show (fmap (second fst) x0)
    forM_ (Map.toList . unPatchMap $ patchMapInitialize x0) $ uncurry handleFinal
    performEvent_ $
      ffor xDiff $ \m -> do
        liftIO $ putTextLn $ "EVT " <> show (fmap (second fst) m)
        forM_ (Map.toList . unPatchMap $ m) $ uncurry handleFinal
    pure never

patchMapInitialize :: Map k v -> PatchMap k v
patchMapInitialize = PatchMap . fmap Just

handleFinal :: MonadIO m => Text -> Maybe (Either (Conflict FilePath ByteString) (FilePath, ByteString)) -> m ()
handleFinal f mv = do
  let k = replaceExtension (toString f) ".html"
      g = "/tmp/g/" <> k
  case mv of
    Just (Left conflict) -> do
      liftIO $ putTextLn $ "CON " <> show conflict
      writeFileBS g $ "<p style='color: red'>" <> show conflict <> "</p>"
    Just (Right (_fp, s)) -> do
      liftIO $ putTextLn $ "WRI " <> toText k
      writeFileBS g $ "<pre>" <> s <> "</pre>"
    Nothing -> do
      liftIO $ putTextLn $ "DEL " <> toText k
      liftIO $ removeFile g

pipeDiscardContent :: (Reflex t, Ord k) => Incremental t (PatchMap k v) -> Incremental t (PatchMap k ())
pipeDiscardContent =
  unsafeMapIncremental void void

pipeFilterExt :: Reflex t => String -> Incremental t (PatchMap FilePath v) -> Incremental t (PatchMap FilePath v)
pipeFilterExt ext =
  unsafeMapIncremental
    (Map.mapMaybeWithKey $ \fs x -> guard (takeExtension fs == ext) >> pure x)
    (PatchMap . Map.mapMaybeWithKey (\fs x -> guard (takeExtension fs == ".md") >> pure x) . unPatchMap)

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

pipeFlattenFsTree ::
  forall t k v.
  (Reflex t, Ord k) =>
  -- | How to flatten the file path.
  (FilePath -> k) ->
  Incremental t (PatchMap FilePath v) ->
  Incremental t (PatchMap k (Either (Conflict FilePath v) (FilePath, v)))
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
        s <- readFileBS f
        pure (f, s)
  fsPatches <- performEvent $
    ffor fsEvents $ \evts ->
      liftIO $ readFilesAsPatchMap evts
  holdIncremental fs0 fsPatches
  where
    readFilesAsPatchMap :: [FSN.Event] -> IO (PatchMap FilePath ByteString)
    readFilesAsPatchMap =
      fmap (PatchMap . Map.fromList . catMaybes) . traverse go
      where
        go = \case
          FSN.Added fp _time False ->
            Just . (fp,) . Just <$> readFileBS fp
          FSN.Modified fp _time False ->
            Just . (fp,) . Just <$> readFileBS fp
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