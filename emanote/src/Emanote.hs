{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Emanote where

import Control.Concurrent.Async (race_)
import Control.Concurrent.STM (newTChanIO, readTChan, writeTChan)
import Emanote.Zk (Zk)
import qualified Emanote.Zk as Zk
import Options.Applicative
import Reflex (Reflex (never))
import Reflex.Host.Headless (MonadHeadlessApp, runHeadlessApp)
import Relude

emanoteMainWith :: (forall t m. MonadHeadlessApp t m => m Zk) -> (Zk -> IO ()) -> IO ()
emanoteMainWith runner f = do
  ready <- newTChanIO @Zk
  race_
    -- Run the Reflex network that will produce the Zettelkasten (Zk)
    ( runHeadlessApp $ do
        zk <- runner
        atomically $ writeTChan ready zk
        pure never
    )
    -- Start consuming the Zk as soon as it is ready.
    ( do
        zk <- atomically $ readTChan ready
        race_
          -- Custom action
          (f zk)
          -- Run TIncremental to process Reflex patches
          (Zk.run zk)
    )
