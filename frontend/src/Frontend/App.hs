{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

module Frontend.App
  ( runApp,
    requestingDynamic,
    requestingDynamicWithRefreshEvent,
    pollRevUpdates,
  )
where

import Common.Api (EmanoteApi (..), EmanoteNet)
import Common.Route
import Control.Monad.Fix (MonadFix)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Constraint.Extras (Has)
import qualified Data.Text as T
import Data.Time.Clock
import Obelisk.Configs (HasConfigs, getTextConfig)
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Reflex.Dom.GadtApi
import Relude

type ValidEnc = Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName

withEndpoint :: HasConfigs m => (Either ApiEndpoint WebSocketEndpoint -> m a) -> m a
withEndpoint f = do
  let enc :: Either Text ValidEnc = checkEncoder fullRouteEncoder
  route <- getTextConfig "common/route"
  mRequestType <- fmap T.strip <$> getTextConfig "frontend/requestType"
  case (enc, route, mRequestType) of
    (Left _, _, _) -> error "Routes are invalid!"
    (_, Nothing, _) -> error "Couldn't load common/route config file"
    (_, _, Nothing) -> error "Couldn't load frontend/requestType config file"
    (Right validEnc, Just host, Just "ws") -> do
      let wsEndpoint =
            Right $
              T.replace "http" "ws" host
                <> renderBackendRoute validEnc (BackendRoute_WebSocket :/ ())
      f wsEndpoint
    (Right validEnc, Just _host, Just "xhr") -> do
      let xhrEndpoint =
            Left $ renderBackendRoute validEnc (BackendRoute_Api :/ ())
      f xhrEndpoint
    (Right _validEnc, Just _host, Just _) -> do
      error "Invalid value in frontend/requestType"

runApp ::
  ( HasConfigs m,
    Routed t (R FrontendRoute) m,
    MonadHold t m,
    MonadFix m,
    Prerender js t m
  ) =>
  RoutedT t (R FrontendRoute) (RequesterT t EmanoteApi (Either Text) m) a ->
  m a
runApp f = do
  withEndpoint $ \endpoint -> do
    r :: Dynamic t (R FrontendRoute) <- askRoute
    startEmanoteNet endpoint $ runRoutedT f r

startEmanoteNet ::
  forall js t m a.
  ( Prerender js t m,
    MonadHold t m,
    MonadFix m,
    Has FromJSON EmanoteApi,
    forall x. ToJSON (EmanoteApi x)
  ) =>
  Either ApiEndpoint WebSocketEndpoint ->
  EmanoteNet t m a ->
  m a
startEmanoteNet endpoint f = do
  rec (x, requests) <- runRequesterT f responses
      responses <- case endpoint of
        Left xhr -> performXhrRequests xhr (requests :: Event t (RequesterData EmanoteApi))
        Right ws -> performWebSocketRequests ws (requests :: Event t (RequesterData EmanoteApi))
  pure x

-- | Like @requesting@, but takes a Dynamic instead.
requestingDynamic ::
  forall a t m js.
  (Requester t m, MonadSample t m, Prerender js t m) =>
  Dynamic t (Request m a) ->
  m (Event t (Response m a))
requestingDynamic reqDyn = do
  requestingDynamicWithRefreshEvent reqDyn never

-- | Like @requestingDynamict@, but allow remaking the request on an event
requestingDynamicWithRefreshEvent ::
  forall a t m js.
  (Requester t m, MonadSample t m, Prerender js t m) =>
  Dynamic t (Request m a) ->
  Event t () ->
  m (Event t (Response m a))
requestingDynamicWithRefreshEvent reqDyn refreshE = do
  r0 <- sample $ current reqDyn
  let rE = updated reqDyn
  requesting <=< fmap switchPromptlyDyn $ do
    -- NOTE: For some strange reason, the getPotBuild must be inside prerender;
    -- otherwise it won't fire for consumption by `requestiong`.
    prerender (pure never) $ do
      pb <- getPostBuild
      pure $
        leftmost
          [ r0 <$ pb,
            rE,
            tag (current reqDyn) refreshE
          ]

-- Polling to fake real-time updates

-- | For each new route change with current rev, poll the backend until a new rev
-- becomes available (in output event).
pollRevUpdates ::
  forall t m rev.
  ( MonadHold t m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadIO m,
    MonadIO (Performable m),
    Requester t m,
    MonadFix m,
    Ord rev
  ) =>
  -- | The request that fetches the current rev
  Request m rev ->
  -- | How to pull the rev out of that request's response.
  (Response m rev -> Maybe rev) ->
  -- | Rev from current page rendering.
  --
  -- This generally coincides with the route event, inasmuch as route change
  -- results in API fetch which returns the rev along with the API data (that
  -- rev is available in the event here)
  --
  -- Polling is enabled only if this event fires.
  Event t rev ->
  m (Event t rev)
pollRevUpdates req respVal currentRevE = do
  timeNow <- liftIO getCurrentTime
  pollE <- tickLossyFrom 1 timeNow currentRevE
  revRefresh <- fmapMaybe respVal <$> requesting (req <$ pollE)
  currentRev <- fmap current $ holdDyn Nothing $ Just <$> currentRevE
  result <-
    holdDyn Nothing $
      Just
        <$> attachWithMaybe
          ( \mt0 tn -> do
              t0 <- mt0
              -- If the server-reported revision (tn) is greater than last page
              -- render's rev (t0), fire an update event with the server's rev.
              guard $ tn > t0
              pure tn
          )
          currentRev
          revRefresh
  fmapMaybe id . updated <$> holdUniqDyn result
