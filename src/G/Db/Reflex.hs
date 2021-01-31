{-# LANGUAGE GADTs #-}

module G.Db.Reflex where

import qualified G.Db as Db
import Reflex

-- | Drain the incremental to the given database.
-- TODO: Refactor out the p1/p2 DRY violation! (using PatchDMap?)
incrementalToDb ::
  forall t m k1 k2 v1 v2 p1 p2 dbPatch.
  ( Reflex t,
    PerformEvent t m,
    MonadSample t m,
    MonadIO m,
    MonadIO (Performable m),
    Ord k1,
    Show k1,
    Ord k2,
    Show k2,
    Patch dbPatch,
    p1 ~ PatchMap k1 v1,
    p2 ~ PatchMap k2 v2
  ) =>
  -- | The database to send the values to.
  Db.Db dbPatch ->
  -- | Function to convert the Incremental's patch to the Db's patch
  --
  -- This function is also called to convert the initial PatchTarget value.
  (p1 -> dbPatch) ->
  (p2 -> dbPatch) ->
  Incremental t p1 ->
  Incremental t p2 ->
  m ()
incrementalToDb db mkPatch1 mkPatch2 i1 i2 = do
  -- Send initial data.
  x0 <- sample $ currentIncremental i1
  y0 <- sample $ currentIncremental i2
  liftIO $ do
    putTextLn $ "INI " <> show (void x0)
    putTextLn $ "INI " <> show (void y0)
    Db.patchDb db $ mkPatch1 (patchMapInitialize x0)
    Db.patchDb db $ mkPatch2 (patchMapInitialize y0)
  -- Send patches.
  let xE = updatedIncremental i1
      yE = updatedIncremental i2
  performEvent_ $
    ffor xE $ \m -> do
      unless (null m) $ do
        liftIO $ do
          putTextLn $ "EVT " <> show (void m)
          Db.patchDb db $ mkPatch1 m
  performEvent_ $
    ffor yE $ \m -> do
      unless (null m) $ do
        liftIO $ do
          putTextLn $ "EVT " <> show (void m)
          Db.patchDb db $ mkPatch2 m
  where
    -- We "cheat" by treating the initial map as a patch map, that "adds" all
    -- initial data.
    patchMapInitialize :: Map k v -> PatchMap k v
    patchMapInitialize = PatchMap . fmap Just
