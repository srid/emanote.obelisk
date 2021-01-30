{-# LANGUAGE GADTs #-}

module G.Db.Reflex where

import qualified G.Db as Db
import Reflex

-- | Drain the incremental to the given database.
incrementalToDb ::
  forall t m k v patch dbPatch.
  ( Reflex t,
    PerformEvent t m,
    MonadSample t m,
    MonadIO m,
    MonadIO (Performable m),
    Ord k,
    Show k,
    Patch dbPatch,
    patch ~ PatchMap k v
  ) =>
  -- | The database to send the values to.
  Db.Db dbPatch ->
  -- | CFunction to convert the Incremental's patch to the Db's patch
  --
  -- This function is also called to convert the initial PatchTarget value.
  (patch -> dbPatch) ->
  Incremental t patch ->
  m ()
incrementalToDb db mkPatch inc = do
  let f = Db.patchDb db . mkPatch
  -- Send initial data.
  x0 <- sample $ currentIncremental inc
  liftIO $ do
    putTextLn $ "INI " <> show (void x0)
    f $ patchMapInitialize x0
  -- Send patches.
  let xE = updatedIncremental inc
  performEvent_ $
    ffor xE $ \m -> do
      liftIO $ do
        putTextLn $ "EVT " <> show (void m)
        f m
  where
    -- We "cheat" by treating the initial map as a patch map, that "adds" all
    -- initial data.
    patchMapInitialize :: Map k v -> PatchMap k v
    patchMapInitialize = PatchMap . fmap Just
