module Backend where

import Common.Route
import Obelisk.Backend
-- import qualified Emanote

backend :: Backend BackendRoute FrontendRoute
backend =
  Backend
    { _backend_run = \serve -> do 
        serve $ \_ -> do 
          -- forkIO $ Emanote.emanoteMain "./doc"
          return (),
      _backend_routeEncoder = fullRouteEncoder
    }
