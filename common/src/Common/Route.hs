{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Functor.Identity
import Data.Text (Text)
import Obelisk.Route
import Obelisk.Route.TH
import Relude

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Api :: BackendRoute ()
  BackendRoute_WebSocket :: BackendRoute ()

-- You can define any routes that will be handled specially by the backend here.
-- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()

-- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

fullRouteEncoder ::
  Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder =
  mkFullRouteEncoder
    (FullRoute_Backend BackendRoute_Missing :/ ())
    ( \case
        BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
        BackendRoute_Api -> PathSegment "api" $ unitEncoder mempty
        BackendRoute_WebSocket -> PathSegment "ws" $ unitEncoder mempty
    )
    ( \case
        FrontendRoute_Main -> PathEnd $ unitEncoder mempty
    )

concat
  <$> mapM
    deriveRouteComponent
    [ ''BackendRoute,
      ''FrontendRoute
    ]
