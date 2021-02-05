module Reflex.TIncremental
  ( TIncremental,
    mirrorIncremental,
    runTIncremental,
    readValue,
  )
where

import Control.Concurrent.STM (TChan)
import qualified Control.Concurrent.STM as STM
import Reflex
import Relude

-- | Represents a reflex @Incremental@ *outside* its network, whilst continuing to
-- support incremental updates.
data TIncremental p = TIncremental
  { _tincremental_patches :: TChan p,
    _tincremental_value :: TVar (PatchTarget p)
  }

readValue :: (MonadIO m) => TIncremental p -> m (PatchTarget p)
readValue TIncremental {..} =
  liftIO $ atomically $ STM.readTVar _tincremental_value

runTIncremental :: Patch p => TIncremental p -> IO ()
runTIncremental TIncremental {..} = do
  forever $
    atomically $ do
      p <- STM.readTChan _tincremental_patches
      x <- STM.readTVar _tincremental_value
      whenJust (apply p x) $ \x' ->
        STM.writeTVar _tincremental_value x'

-- | Mirror the Incremental outside of the Reflex network. Use `runTIncremental`
-- to effectuate the mirror.
mirrorIncremental ::
  ( Reflex t,
    Patch p,
    MonadIO m,
    MonadIO (Performable m),
    PerformEvent t m,
    MonadSample t m
  ) =>
  Incremental t p ->
  m (TIncremental p)
mirrorIncremental inc = do
  x0 <- sample $ currentIncremental inc
  _tincremental_value <- liftIO $ STM.newTVarIO x0
  _tincremental_patches <- liftIO STM.newTChanIO
  let xE = updatedIncremental inc
  performEvent_ $
    ffor xE $ \m -> do
      atomically $ STM.writeTChan _tincremental_patches m
  pure TIncremental {..}
