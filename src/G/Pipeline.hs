{-# LANGUAGE GADTs #-}

module G.Pipeline (run) where

import qualified Commonmark.Syntax as CM
import Data.Conflict (Conflict (..))
import qualified Data.Conflict as Conflict
import qualified Data.Conflict.Patch as Conflict
import qualified Data.Map as Map
import Data.Tagged (Tagged (..))
import qualified G.Db as Db
import qualified G.Db.Reflex as Db
import qualified G.Db.Types.Zk.Patch as Zk
import G.FileSystem (directoryTreeIncremental)
import qualified G.Markdown as M
import qualified G.Markdown.WikiLink as M
import Reflex
import Reflex.Host.Headless (MonadHeadlessApp)
import System.FilePath (dropExtension, takeExtension, takeFileName)
import qualified Text.Mustache as Mustache
import Text.Pandoc.Definition (Pandoc)
import qualified Text.Pandoc.LinkContext as LC

run :: MonadHeadlessApp t m => FilePath -> Db.Db Zk.ZkPatch -> m (Event t ())
run inputDir db = do
  input <- directoryTreeIncremental [".*/**"] inputDir
  let mdOut =
        input
          & pipeFilterFilename (\fn -> takeExtension fn == ".md")
          & pipeFlattenFsTree (Tagged . toText . dropExtension . takeFileName)
          & pipeParseMarkdown (M.wikiLinkSpec <> M.markdownSpec)
          & pipeExtractLinks
      htmlOut =
        input
          & pipeFilterFilename (== "index.html")
          & pipeLoadTemplates
  Db.incrementalToDb db Zk.mkZkPatch1 Zk.mkZkPatch2 mdOut htmlOut
  pure never

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

pipeLoadTemplates ::
  Reflex t =>
  Incremental t (PatchMap FilePath ByteString) ->
  Incremental t (PatchMap FilePath (Either Text Mustache.Template))
pipeLoadTemplates =
  let f n = first (show @Text) . Mustache.compileTemplate n . decodeUtf8 @Text @ByteString
   in unsafeMapIncremental
        (Map.mapWithKey f)
        (PatchMap . Map.mapWithKey (fmap . f) . unPatchMap)

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

pipeExtractLinks ::
  forall t f g h.
  (Reflex t, Functor f, Functor g, Functor h) =>
  Incremental t (PatchMap M.WikiLinkID (f (g (h Pandoc)))) ->
  Incremental t (PatchMap M.WikiLinkID (f (g (h ([((M.WikiLinkLabel, M.WikiLinkContext), M.WikiLinkID)], Pandoc)))))
pipeExtractLinks = do
  unsafeMapIncremental
    (Map.map $ (fmap . fmap . fmap) f)
    (PatchMap . Map.map ((fmap . fmap . fmap . fmap) f) . unPatchMap)
  where
    f doc =
      let links = LC.queryLinksWithContext doc
          getTitleAttr =
            Map.lookup "title" . Map.fromList
       in ( (\(url, (getTitleAttr -> tit, ctx)) -> first (,ctx) <$> M.parseWikiLinkUrl tit url)
              `fmapMaybe` Map.toList links,
            doc
          )

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
