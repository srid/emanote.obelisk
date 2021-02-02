{-# LANGUAGE GADTs #-}

module Emanote.Pipeline (run) where

import qualified Commonmark.Syntax as CM
import Data.Conflict (Conflict (..))
import qualified Data.Conflict as Conflict
import qualified Data.Conflict.Patch as Conflict
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tagged (Tagged (..))
import qualified Data.Text as T
import Emanote.FileSystem (PathContent (..), directoryTreeIncremental)
import qualified Emanote.Graph as G
import qualified Emanote.Graph.Patch as G
import qualified Emanote.Markdown as M
import qualified Emanote.Markdown.WikiLink as M
import Emanote.Zk (Zk (Zk))
import Reflex
import Reflex.Host.Headless (MonadHeadlessApp)
import qualified Reflex.TIncremental as TInc
import System.FilePath (dropExtension, takeDirectory, takeExtension, takeFileName)
import qualified Text.Mustache as Mustache
import Text.Pandoc.Definition (Pandoc)
import qualified Text.Pandoc.LinkContext as LC

run :: MonadHeadlessApp t m => FilePath -> m Zk
run inputDir = do
  input' <- directoryTreeIncremental [".*/**"] inputDir
  -- TODO: Deal with directory events sensibly, instead of ignoring them.
  let input = input' & pipeFilesOnly
  logInputChanges input
  let pandocOut =
        input
          & pipeFilterFilename (\fn -> takeExtension fn == ".md")
          & pipeInjectDirZettels
          & pipeFlattenFsTree (Tagged . toText . dropExtension . takeFileName)
          & pipeParseMarkdown (M.wikiLinkSpec <> M.markdownSpec)
      graphOut =
        pandocOut
          & pipeExtractLinks
          & pipeGraph
      htmlOut =
        input
          & pipeFilterFilename ("templates/" `isPrefixOf`)
          & pipeLoadTemplates
  Zk
    <$> TInc.mirrorIncremental pandocOut
    <*> TInc.mirrorIncremental graphOut
    <*> TInc.mirrorIncremental htmlOut

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

-- | Treat directories as .md files.
--
-- - Create `$folder.md` (unless exists) for every $folder containing .md files
-- - Append the text `Child of #[[Parent Folder]]` at the end of every .md file
pipeInjectDirZettels :: Reflex t => Incremental t (PatchMap FilePath ByteString) -> Incremental t (PatchMap FilePath ByteString)
pipeInjectDirZettels inc = do
  inc
    & unsafeMapIncrementalWithOldValue
      (fmapTargetAsPatch createFiles)
      createFiles
    & unsafeMapIncremental
      (fmapTargetAsPatch $ const writeConn)
      writeConn
  where
    fmapTargetAsPatch f =
      Map.mapMaybe id . unPatchMap . f Map.empty . PatchMap . fmap Just
    -- Phase 1: just create an empty $folder.md if it doesn't exist.
    createFiles :: Map FilePath ByteString -> PatchMap FilePath ByteString -> PatchMap FilePath ByteString
    createFiles oldVal p =
      let parZettels =
            Set.toList $
              Set.fromList $
                -- TODO: Handle deletion
                flip concatMap (Map.keys $ patchMapNewElementsMap p) $ \fp ->
                  parents fp <&> \x -> (x, x <> ".md")
          parZettelsPatch :: PatchMap FilePath ByteString =
            PatchMap $
              Map.fromList $
                fforMaybe parZettels $ \(dirP, fp) ->
                  case Map.lookup fp oldVal of
                    Just _ ->
                      Nothing
                    Nothing ->
                      Just (fp, Just $ dirZettelBody dirP)
       in p <> parZettelsPatch
    dirZettelBody p =
      "Directory Zettel, autocreated for folder: `" <> encodeUtf8 p <> "`"
    parents fp =
      let par = takeDirectory fp
       in ffor (inits $ toString <$> T.splitOn "/" (toText par)) $ \s ->
            if null s
              then "index"
              else toString (T.intercalate "/" $ fmap toText s)
    -- Phase 2: inject wiki-links to effectuate graph connections.
    writeConn :: PatchMap FilePath ByteString -> PatchMap FilePath ByteString
    writeConn p =
      PatchMap $
        flip Map.mapWithKey (unPatchMap p) $ \fp -> \case
          Nothing -> Nothing
          Just s ->
            let parID = takeFileName (takeDirectory fp)
             in Just $ s <> if parID == "." then connSuffix "index" else connSuffix parID
    connSuffix (target :: String) =
      -- Apparently \n doesn't work here
      encodeUtf8 $ "\r\r---\r" <> "Folder: #[[" <> target <> "]]"

pipeLoadTemplates ::
  Reflex t =>
  Incremental t (PatchMap FilePath ByteString) ->
  Incremental t (PatchMap FilePath (Either Text Mustache.Template))
pipeLoadTemplates =
  let f n = first (show @Text) . Mustache.compileTemplate n . decodeUtf8 @Text @ByteString
   in unsafeMapIncremental
        (Map.mapWithKey f)
        (PatchMap . Map.mapWithKey (fmap . f) . unPatchMap)

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
  Incremental t (PatchMap M.WikiLinkID [((M.WikiLinkLabel, M.WikiLinkContext), M.WikiLinkID)])
pipeExtractLinks = do
  unsafeMapIncremental
    (Map.map $ (concatMap . concatMap . concatMap) f)
    (PatchMap . Map.map ((fmap . concatMap . concatMap . concatMap) f) . unPatchMap)
  where
    f doc =
      let links = LC.queryLinksWithContext doc
          getTitleAttr =
            Map.lookup "title" . Map.fromList
       in (\(url, (getTitleAttr -> tit, ctx)) -> first (,ctx) <$> M.parseWikiLinkUrl tit url)
            `fmapMaybe` Map.toList links

pipeGraph ::
  forall t.
  (Reflex t) =>
  Incremental t (PatchMap M.WikiLinkID [((M.WikiLinkLabel, M.WikiLinkContext), M.WikiLinkID)]) ->
  Incremental t G.PatchGraph
pipeGraph = do
  unsafeMapIncremental
    (fromMaybe G.empty . flip apply G.empty . f . PatchMap . fmap Just)
    f
  where
    f ::
      PatchMap M.WikiLinkID [((M.WikiLinkLabel, M.WikiLinkContext), M.WikiLinkID)] ->
      G.PatchGraph
    f p =
      G.PatchGraph . unPatchMap $
        fmap (first one) <$> p

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
