{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

module Frontend.App
  ( runApp,
    mkBackendRequest,
    withBackendResponse,
    EmanoteRequester,
    requestingDynamic,
    pollRevUpdates,
  )
where

import Common.Api (EmanoteApi (..), EmanoteNet)
import Common.Route
import Control.Monad.Fix (MonadFix)
import Data.Aeson (ToJSON (..))
import qualified Data.Text as T
import Data.Time.Clock
import Obelisk.Configs (HasConfigs, getTextConfig)
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Reflex.Dom.GadtApi
import Relude

-- Application top-level runner
-- ----------------------------

type ValidEnc = Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName

runApp ::
  ( HasConfigs m,
    Routed t (R FrontendRoute) m,
    MonadHold t m,
    MonadFix m,
    Prerender js t m
  ) =>
  RoutedT t (R FrontendRoute) (RequesterT t EmanoteApi (Either Text) m) a ->
  m a
runApp w = do
  withEndpoint $ \endpoint -> do
    r :: Dynamic t (R FrontendRoute) <- askRoute
    startEmanoteNet endpoint $ runRoutedT w r
  where
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

    startEmanoteNet ::
      forall js t m a.
      ( Prerender js t m,
        MonadHold t m,
        MonadFix m,
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

-- Widget-level request/response handling
-- These provide a higher abstraction over the type/functions below
-- -----------------------------------------

-- Make a request to the backend.
mkBackendRequest ::
  forall r a t m js.
  ( Prerender js t m,
    MonadHold t m,
    EmanoteRequester t m
  ) =>
  -- | Refresh the request on firing this event.
  Event t () ->
  -- | What request to make.
  Dynamic t (Request m a) ->
  -- Return the response, along with an indicator of "still making request" state.
  RoutedT t r m (Dynamic t Bool, Event t (Response m a))
mkBackendRequest requestAgain req = do
  resp <- requestingDynamicWithRefreshEvent req requestAgain
  waiting <-
    holdDyn True $
      leftmost
        [ fmap (const True) (updated req),
          fmap (const False) resp
        ]
  pure (waiting, resp)


-- Handle a response event from backend, and invoke the given widget for the
-- actual result.
--
-- This function does loading state and error handling.
withBackendResponse ::
  ( DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m
  ) =>
  -- | Response event from backend
  Event t (Either Text result) ->
  -- | The value to return when the result is not yet available or successful.
  Dynamic t v ->
  -- | Widget to render when the successful result becomes available
  (Dynamic t result -> m (Dynamic t v)) ->
  m (Dynamic t v)
withBackendResponse resp v0 f = do
  mresp <- maybeDyn =<< holdDyn Nothing (Just <$> resp)
  fmap join . holdDyn v0 <=< dyn $
    ffor mresp $ \case
      Nothing -> do
        loader
        pure v0
      Just resp' -> do
        eresp <- eitherDyn resp'
        fmap join . holdDyn v0 <=< dyn $
          ffor eresp $ \case
            Left errDyn -> do
              dynText $ show <$> errDyn
              pure v0
            Right result ->
              f result
  where
    loader :: DomBuilder t m => m ()
    loader =
      divClass "grid grid-cols-3 ml-0 pl-0 content-evenly" $ do
        divClass "col-start-1 col-span-3 h-16" blank
        divClass "col-start-2 col-span-1 place-self-center p-4 h-full bg-black text-white rounded" $
          text "Loading..."

-- Requester type and functions
-- Uses reflex-gadt-api underneath
-- -------------------------------

type EmanoteRequester t m =
  ( Response m ~ Either Text,
    Request m ~ EmanoteApi,
    Requester t m
  )

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
