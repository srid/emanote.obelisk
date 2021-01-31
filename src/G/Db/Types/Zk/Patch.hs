{-# LANGUAGE TypeFamilies #-}

module G.Db.Types.Zk.Patch where

import Data.Conflict (Conflict)
import Data.Default
import G.Db.Types.Zk (Zettel, Zk (..))
import G.Graph.Patch (PatchGraph (..))
import qualified G.Markdown as M
import qualified G.Markdown.WikiLink as M
import Reflex (PatchMap (unPatchMap))
import Reflex.Patch.Class (Patch (..))
import qualified Text.Mustache.Types as Mustache
import Text.Pandoc.Definition (Pandoc)

data ZkPatch = ZkPatch
  { _zkPatch_zettels :: PatchMap M.WikiLinkID Zettel,
    _zkPatch_graph :: PatchGraph,
    _zkPatch_htmlTemplate :: PatchMap FilePath (Either Text Mustache.Template)
  }

instance Default ZkPatch where
  def = ZkPatch mempty (PatchGraph mempty) mempty

instance Patch ZkPatch where
  type PatchTarget ZkPatch = Zk
  apply ZkPatch {..} Zk {..} =
    let mzs = apply _zkPatch_zettels _zk_zettels
        mg = apply _zkPatch_graph _zk_graph
        mhtml = apply _zkPatch_htmlTemplate _zk_htmlTemplate
     in if isJust mzs || isJust mg || isJust mhtml
          then Just $ Zk (fromMaybe _zk_zettels mzs) (fromMaybe _zk_graph mg) (fromMaybe _zk_htmlTemplate mhtml)
          else Nothing

-- Why two similar functions? See the comment in `incrementalToDb`

mkZkPatch1 ::
  PatchMap
    M.WikiLinkID
    ( Either
        (Conflict FilePath ByteString)
        ( FilePath,
          Either
            M.ParserError
            ([((M.WikiLinkLabel, M.WikiLinkContext), M.WikiLinkID)], Pandoc)
        )
    ) ->
  ZkPatch
mkZkPatch1 p =
  let _zkPatch_zettels =
        (fmap . fmap . fmap . fmap) snd p
      _zkPatch_graph =
        PatchGraph . unPatchMap $
          p
            <&> concat . \case
              Right (_, Right (ids, _)) -> [first one <$> ids]
              _ -> []
   in def
        { _zkPatch_zettels = _zkPatch_zettels,
          _zkPatch_graph = _zkPatch_graph
        }

mkZkPatch2 ::
  PatchMap FilePath (Either Text Mustache.Template) ->
  ZkPatch
mkZkPatch2 p =
  def {_zkPatch_htmlTemplate = p}
