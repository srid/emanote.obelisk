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

import Control.Category
import Control.Lens.Combinators
import Data.Tagged
import Data.Text (Text)
import qualified Data.Text as T
import Emanote.Markdown.WikiLink (WikiLinkID)
import Obelisk.Route
import Obelisk.Route.TH
import Relude hiding (id, (.))

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Api :: BackendRoute ()
  BackendRoute_WebSocket :: BackendRoute ()

-- You can define any routes that will be handled specially by the backend here.
-- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_Note :: FrontendRoute WikiLinkID

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
        FrontendRoute_Note -> PathSegment "-" $ singlePathSegmentEncoder . wikiLinkEncoder
    )
  where
    wikiLinkEncoder :: (Applicative check, Applicative parse) => Encoder check parse WikiLinkID Text
    wikiLinkEncoder = viewEncoder wikiLinkIso
    wikiLinkIso :: Iso' WikiLinkID Text
    wikiLinkIso = iso (prettify . untag) (Tagged . deprettify)
      where
        prettify = T.replace " " "_"
        deprettify = T.replace "_" " " -- TODO: disambiguate, by creating 'Slug' map in backend

concat
  <$> mapM
    deriveRouteComponent
    [ ''BackendRoute,
      ''FrontendRoute
    ]
