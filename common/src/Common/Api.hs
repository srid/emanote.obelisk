{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Common.Api where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.GADT.Show.TH (deriveGShow)
import qualified Emanote.Markdown.WikiLink as EM
import Emanote.Zk.Type (Zettel)
import Reflex.Dom.Core
import Relude

data EmanoteApi :: * -> * where
  EmanoteApi_GetNotes :: EmanoteApi [EM.WikiLinkID]
  EmanoteApi_Note :: EM.WikiLinkID -> EmanoteApi (Maybe Zettel)

deriveGShow ''EmanoteApi
deriveJSONGADT ''EmanoteApi
deriveArgDict ''EmanoteApi

deriving instance Show (EmanoteApi [EM.WikiLinkID])

deriving instance Show (EmanoteApi (Maybe Zettel))

type EmanoteNet t m = RequesterT t EmanoteApi (Either Text) m
