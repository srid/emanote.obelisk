module Backend where

import Common.Route
import Obelisk.Backend

backend :: Backend BackendRoute FrontendRoute
backend =
  Backend
    { _backend_run = \serve -> do 
        serve $ \_ -> do 
          return (),
      _backend_routeEncoder = fullRouteEncoder
    }
