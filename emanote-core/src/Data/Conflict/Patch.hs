module Data.Conflict.Patch where

import Data.Conflict (Conflict (..), increaseConflict, lowerConflict, resolveConflicts)
import qualified Data.Map as Map
import Reflex (PatchMap (..), fmapMaybe)
import Relude
import Relude.Extra (groupBy)

applyPatch ::
  (Ord k, Eq identifier) =>
  (identifier -> k) ->
  Map identifier v ->
  PatchMap identifier v ->
  PatchMap k (Either (Conflict identifier v) (identifier, v))
applyPatch toKey m p =
  -- Ugly, but works (TM)
  -- TODO: Write tests spec, and then refactor.
  let m' = resolveConflicts toKey m
   in PatchMap $
        flip Map.mapWithKey (groupBy (toKey . fst) (Map.toList (unPatchMap p))) $ \k grouped -> case grouped of
          (fp, val) :| [] -> case val of
            Nothing ->
              -- Deletion event
              case Map.lookup k m' of
                Nothing -> Nothing
                Just (Left conflict) ->
                  case lowerConflict fp conflict of
                    Left prev ->
                      -- Conflict resolved; return the previous data.
                      Just $ Right prev
                    Right c2 ->
                      -- Conflict still exists, but with one less file.
                      Just (Left c2)
                Just (Right _v) ->
                  Nothing
            Just v ->
              -- Modification/addition event
              case Map.lookup k m' of
                Nothing ->
                  Just $ Right (fp, v)
                Just (Left conflict) ->
                  Just $ Left $ increaseConflict fp v conflict
                Just (Right (oldFp, oldVal)) ->
                  if oldFp == fp
                    then Just $ Right (fp, v)
                    else Just $ Left $ Conflict (oldFp, oldVal) ((fp, v) :| [])
          conflictingPatches ->
            let exists = fmapMaybe (\(a, mb) -> (a,) <$> mb) (toList conflictingPatches)
             in case Map.lookup k m' of
                  Nothing ->
                    case exists of
                      [] -> Nothing
                      [v] -> Just (Right v)
                      (v1 : v2 : vs) -> Just $ Left $ Conflict v1 (v2 :| vs)
                  Just (Left conflict) ->
                    Just $ Left $ foldl' (flip $ uncurry increaseConflict) conflict exists
                  Just (Right old) ->
                    case exists of
                      [] -> Nothing
                      [v] -> Just (Right v)
                      (v1 : v2 : vs) -> Just $ Left $ Conflict old (v1 :| v2 : vs)
