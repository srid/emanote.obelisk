{-# LANGUAGE TypeFamilies #-}

module G.Db.Types.Zk.Patch where

import Data.Conflict (Conflict)
import G.Db.Types.Zk (Zettel, Zk (..))
import G.Graph.Patch (PatchGraph (..))
import qualified G.Markdown as M
import qualified G.Markdown.WikiLink as M
import Reflex (PatchMap (unPatchMap))
import Reflex.Patch.Class
import Text.Pandoc.Definition (Pandoc)

data ZkPatch = ZkPatch
  { _zkPatch_zettels :: PatchMap M.WikiLinkID Zettel,
    _zkPatch_graph :: PatchGraph
  }

instance Patch ZkPatch where
  type PatchTarget ZkPatch = Zk
  apply ZkPatch {..} Zk {..} =
    let mzs = apply _zkPatch_zettels _zk_zettels
        mg = apply _zkPatch_graph _zk_graph
     in if isJust mzs || isJust mg
          then Just $ Zk (fromMaybe _zk_zettels mzs) (fromMaybe _zk_graph mg)
          else Nothing

mkZkPatch ::
  PatchMap
    M.WikiLinkID
    ( Either
        (Conflict FilePath ByteString)
        ( FilePath,
          Either
            M.ParserError
            ([(M.WikiLinkLabel, M.WikiLinkID)], Pandoc)
        )
    ) ->
  ZkPatch
mkZkPatch p =
  let _zkPatch_zettels =
        (fmap . fmap . fmap . fmap) snd p
      _zkPatch_graph =
        PatchGraph . unPatchMap $
          p
            <&> concat . \case
              Right (_, Right (ids, _)) -> [first one <$> ids]
              _ -> []
   in ZkPatch {..}
