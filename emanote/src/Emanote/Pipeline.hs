{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Emanote.Pipeline (run, runNoMonitor) where

import qualified Algebra.Graph.Labelled.AdjacencyMap as AM
import qualified Algebra.Graph.Labelled.AdjacencyMap.Patch as G
import qualified Commonmark.Syntax as CM
import Data.Conflict (Conflict (..))
import qualified Data.Conflict as Conflict
import qualified Data.Conflict.Patch as Conflict
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tagged (Tagged (..), untag)
import Emanote.FileSystem (PathContent (..))
import qualified Emanote.FileSystem as FS
import qualified Emanote.Graph as G
import qualified Emanote.Markdown as M
import qualified Emanote.Markdown.WikiLink as M
import qualified Emanote.Markdown.WikiLink.Parser as M
import Emanote.Zk (Zk (Zk))
import Reflex hiding (mapMaybe)
import Reflex.Host.Headless (MonadHeadlessApp)
import qualified Reflex.TIncremental as TInc
import Relude
import System.FilePath (dropExtension, takeExtension, takeFileName)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import Text.Pandoc.Definition (Pandoc)
import qualified Text.Pandoc.LinkContext as LC

-- | Like `run`, but stops observing for file changes after the initial read
runNoMonitor :: MonadHeadlessApp t m => FilePath -> m Zk
runNoMonitor x = do
  liftIO $ putStrLn "Running pipeline in read-only mode"
  run' False x

run :: MonadHeadlessApp t m => FilePath -> m Zk
run x = do
  liftIO $ putStrLn "Running pipeline in monitor mode"
  run' True x

run' :: MonadHeadlessApp t m => Bool -> FilePath -> m Zk
run' monitor inputDir = do
  input' <-
    if monitor
      then FS.directoryTreeIncremental [".*/**"] inputDir
      else flip holdIncremental never =<< FS.directoryTree [".*/**"] inputDir
  -- TODO: Deal with directory events sensibly, instead of ignoring them.
  let input = input' & pipeFilesOnly
  logInputChanges input
  let pandocOut =
        input
          & pipeFilterFilename (\fn -> takeExtension fn == ".md")
          & pipeFlattenFsTree (Tagged . toText . dropExtension . takeFileName)
          & pipeParseMarkdown (M.wikiLinkSpec <> M.markdownSpec)
      graphOut =
        pandocOut
          & pipeExtractLinks
          & pipeGraph
          & pipeCreateCalendar
  Zk
    <$> TInc.mirrorIncremental pandocOut
    <*> TInc.mirrorIncremental graphOut
    <*> newTVarIO 0

pipeFilesOnly :: Reflex t => Incremental t (PatchMap FilePath PathContent) -> Incremental t (PatchMap FilePath ByteString)
pipeFilesOnly =
  unsafeMapIncremental
    (Map.mapMaybe getFileContent)
    (PatchMap . Map.mapMaybe (traverse getFileContent) . unPatchMap)
  where
    getFileContent = \case
      PathContent_File s -> Just s
      _ -> Nothing

logInputChanges :: (PerformEvent t m, MonadIO (Performable m)) => Incremental t (PatchMap FilePath a) -> m ()
logInputChanges input =
  performEvent_ $
    ffor (updatedIncremental input) $ \(void -> m) ->
      forM_ (Map.toList $ unPatchMap m) $ \(fp, mval) -> do
        let mark = maybe "-" (const "*") mval
        liftIO $ putStr $ mark <> " "
        liftIO $ putStrLn fp

pipeFilterFilename ::
  Reflex t =>
  (FilePath -> Bool) ->
  Incremental t (PatchMap FilePath v) ->
  Incremental t (PatchMap FilePath v)
pipeFilterFilename selectFile =
  let f :: FilePath -> v -> Maybe v
      f = \fs x -> guard (selectFile fs) >> pure x
   in unsafeMapIncremental
        (Map.mapMaybeWithKey f)
        (PatchMap . Map.mapMaybeWithKey f . unPatchMap)

pipeFlattenFsTree ::
  forall t v.
  (Reflex t) =>
  -- | How to flatten the file path.
  (FilePath -> M.WikiLinkID) ->
  Incremental t (PatchMap FilePath v) ->
  Incremental t (PatchMap M.WikiLinkID (Either (Conflict FilePath v) (FilePath, v)))
pipeFlattenFsTree toKey = do
  unsafeMapIncrementalWithOldValue
    (Conflict.resolveConflicts toKey)
    (Conflict.applyPatch toKey)

pipeParseMarkdown ::
  (Reflex t, Functor f, Functor g, M.MarkdownSyntaxSpec m il bl) =>
  CM.SyntaxSpec m il bl ->
  Incremental t (PatchMap M.WikiLinkID (f (g ByteString))) ->
  Incremental t (PatchMap M.WikiLinkID (f (g (Either M.ParserError Pandoc))))
pipeParseMarkdown spec =
  unsafeMapIncremental
    (Map.mapWithKey $ \fID -> (fmap . fmap) (parse fID))
    (PatchMap . Map.mapWithKey ((fmap . fmap . fmap) . parse) . unPatchMap)
  where
    parse :: M.WikiLinkID -> ByteString -> Either M.ParserError Pandoc
    parse (Tagged (toString -> fn)) = M.parseMarkdown spec fn . decodeUtf8

pipeExtractLinks ::
  forall t f g h.
  (Reflex t, Functor f, Functor g, Functor h, Foldable f, Foldable g, Foldable h) =>
  Incremental t (PatchMap M.WikiLinkID (f (g (h Pandoc)))) ->
  Incremental t (PatchMap M.WikiLinkID [(M.WikiLink, M.WikiLinkID)])
pipeExtractLinks = do
  unsafeMapIncremental
    (Map.map $ (concatMap . concatMap . concatMap) f)
    (PatchMap . Map.map ((fmap . concatMap . concatMap . concatMap) f) . unPatchMap)
  where
    f doc =
      let linkMap = LC.queryLinksWithContext doc
          getTitleAttr =
            Map.lookup "title" . Map.fromList
       in concat $
            ffor (Map.toList linkMap) $ \(url, urlLinks) -> do
              fforMaybe (toList urlLinks) $ \(getTitleAttr -> tit, ctx) -> do
                (lbl, wId) <- M.parseWikiLinkUrl tit url
                pure (M.mkWikiLink lbl ctx, wId)

pipeGraph ::
  forall t.
  (Reflex t) =>
  Incremental t (PatchMap M.WikiLinkID [(M.WikiLink, M.WikiLinkID)]) ->
  Incremental t (G.PatchGraph G.E G.V)
pipeGraph = do
  unsafeMapIncremental
    (fromMaybe AM.empty . flip apply AM.empty . f . PatchMap . fmap Just)
    f
  where
    f ::
      PatchMap M.WikiLinkID [(M.WikiLink, M.WikiLinkID)] ->
      G.PatchGraph G.E G.V
    f p =
      let pairs = Map.toList $ unPatchMap p
       in G.PatchGraph $
            pairs <&> \(k, mes) ->
              case mes of
                Nothing ->
                  G.ModifyGraph_RemoveVertexWithoutPredecessors k
                Just es ->
                  G.ModifyGraph_ReplaceVertexWithSuccessors k (first one <$> es)

-- | Tag daily notes with month zettels ("2020-02"), which are tagged further
-- with year zettels ("2020").
pipeCreateCalendar ::
  Reflex t =>
  Incremental t (G.PatchGraph G.E G.V) ->
  Incremental t (G.PatchGraph G.E G.V)
pipeCreateCalendar =
  unsafeMapIncremental
    (fromMaybe AM.empty . flip apply AM.empty . f . G.asPatchGraph)
    f
  where
    f :: G.PatchGraph G.E G.V -> G.PatchGraph G.E G.V
    f diff =
      let liftNote :: M.Parsec Void Text Text -> G.PatchGraph G.E G.V -> G.PatchGraph G.E G.V
          liftNote wIdParser d =
            G.PatchGraph $
              flip mapMaybe (Set.toList $ G.modifiedOrAddedVertices d) $ \wId -> do
                (Tagged -> parent) <- parse wIdParser (untag wId)
                pure $ G.ModifyGraph_AddEdge (one $ (M.WikiLink M.WikiLinkLabel_Tag mempty)) wId parent
          monthDiff = liftNote monthFromDate diff
          -- Include monthDiff here, so as to 'lift' those ghost month zettels further.
          yearDiff = liftNote yearFromMonth (diff <> monthDiff)
       in mconcat
            [ diff,
              monthDiff,
              yearDiff
            ]
    yearFromMonth :: M.Parsec Void Text Text
    yearFromMonth = do
      year <- num 4 <* dash
      _month <- num 2
      pure year
    monthFromDate :: M.Parsec Void Text Text
    monthFromDate = do
      year <- num 4 <* dash
      month <- num 2 <* dash
      _day <- num 2
      pure $ year <> "-" <> month
    dash = M.char '-'
    num n =
      toText <$> M.count n M.digitChar
    parse :: forall e s r. (M.Stream s, Ord e) => M.Parsec e s r -> s -> Maybe r
    parse p =
      M.parseMaybe (p <* M.eof)

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
