module Backend where

import Common.Route
import Control.Concurrent.Async (race_)
import qualified Data.Map.Strict as Map
import Data.Text as T
import qualified Emanote
import Obelisk.Backend
import Obelisk.ExecutableConfig.Lookup
import Relude

backend :: Backend BackendRoute FrontendRoute
backend =
  Backend
    { _backend_run = \serve -> do
        configs <- getConfigs
        let getCfg k =
              maybe (error $ "Missing " <> k) (T.strip . decodeUtf8) $ Map.lookup k configs
            notesDir = getCfg "backend/notesDir"
        race_ 
           (Emanote.emanoteMain (toString notesDir))
           $ serve $ \_ -> do
              return (),
      _backend_routeEncoder = fullRouteEncoder
    }
