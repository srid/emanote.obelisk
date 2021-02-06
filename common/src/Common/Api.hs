{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Common.Api where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Constraint.Extras (Has)
import Data.Constraint.Extras.TH (deriveArgDict)
import qualified Emanote.Markdown.WikiLink as EM
import Reflex.Dom.Core
import Reflex.Dom.GadtApi
import Relude

data EmanoteApi :: * -> * where
  EmanoteApi_GetNotes :: EmanoteApi [EM.WikiLinkID]

deriveJSONGADT ''EmanoteApi
deriveArgDict ''EmanoteApi

type EmanoteNet t m = RequesterT t EmanoteApi (Either Text) m
