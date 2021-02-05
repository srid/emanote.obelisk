module Backend where

import Common.Route
import Control.Concurrent (forkIO)
import qualified Emanote
import Obelisk.Backend
import Relude

backend :: Backend BackendRoute FrontendRoute
backend =
  Backend
    { _backend_run = \serve -> do
        serve $ \_ -> do
          liftIO $ forkIO $ Emanote.emanoteMain "./doc"
          return (),
      _backend_routeEncoder = fullRouteEncoder
    }
