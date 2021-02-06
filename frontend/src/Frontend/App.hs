{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

module Frontend.App (runApp, requestingDynamic) where

import Common.Api (EmanoteApi (..), EmanoteNet)
import Common.Route
import Control.Monad.Fix (MonadFix)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Constraint.Extras (Has)
import qualified Data.Text as T
import Obelisk.Configs (HasConfigs, getTextConfig)
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Reflex.Dom.GadtApi
import Relude

type ValidEnc = Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName

withEncoderAndRoute :: HasConfigs m => (ValidEnc -> Text -> m a) -> m a
withEncoderAndRoute f = do
  let enc :: Either Text ValidEnc = checkEncoder fullRouteEncoder
  route <- getTextConfig "common/route"
  case (enc, route) of
    (Left _, _) -> error "Routes are invalid!"
    (_, Nothing) -> error "Couldn't load common/route config file"
    (Right validEnc, Just host) -> do
      f validEnc host

runApp ::
  ( HasConfigs m,
    Routed t (R FrontendRoute) m,
    MonadHold t m,
    MonadFix m,
    Prerender js t m
  ) =>
  RoutedT t (R FrontendRoute) (RequesterT t EmanoteApi (Either Text) m) () ->
  m ()
runApp f = do
  withEncoderAndRoute $ \validEnc host -> do
    -- Websocket vs API endpoint. Pick one, during experimental phase.
    let endpoint =
          Right $
            T.replace "http" "ws" host
              <> renderBackendRoute validEnc (BackendRoute_WebSocket :/ ())
        _endpoint =
          Left $ renderBackendRoute validEnc (BackendRoute_Api :/ ())
    r :: Dynamic t (R FrontendRoute) <- askRoute
    startEmanoteNet endpoint $ runRoutedT f r

startEmanoteNet ::
  forall js t m.
  ( Prerender js t m,
    MonadHold t m,
    MonadFix m,
    Has FromJSON EmanoteApi,
    forall a. ToJSON (EmanoteApi a)
  ) =>
  Either ApiEndpoint WebSocketEndpoint ->
  EmanoteNet t m () ->
  m ()
startEmanoteNet endpoint f = do
  rec (_, requests) <- runRequesterT f responses
      responses <- case endpoint of
        Left xhr -> performXhrRequests xhr (requests :: Event t (RequesterData EmanoteApi))
        Right ws -> performWebSocketRequests ws (requests :: Event t (RequesterData EmanoteApi))
  pure ()

-- | Like @requesting@, but takes a Dynamic instead.
requestingDynamic ::
  (Requester t m, MonadSample t m, Prerender js t m) =>
  Dynamic t (Request m a) ->
  m (Event t (Response m a))
requestingDynamic reqDyn = do
  r0 <- sample $ current reqDyn
  let rE = updated reqDyn
  requesting <=< fmap switchPromptlyDyn $ do
    -- NOTE: For some strange reason, the getPotBuild must be inside prerender;
    -- otherwise it won't fire for consumption by `requestiong`.
    prerender (pure never) $ do
      pb <- getPostBuild
      pure $ leftmost [r0 <$ pb, rE]
