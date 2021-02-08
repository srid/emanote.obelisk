{-# LANGUAGE DeriveAnyClass #-}
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
import qualified Emanote.Zk.Type as Zk
import Reflex.Dom.Core
import Relude
import Text.Pandoc.Definition

data Note = Note
  { _note_wikiLinkID :: EM.WikiLinkID,
    _note_wikiLinkUrl :: Text,
    -- | A note may correspond to a non-existant zettel (due to simply being
    -- linked), and so can be `Nothing`.
    _note_zettel :: Maybe Zettel,
    _note_backlinks :: [LinkContext],
    _note_downlinks :: [LinkContext],
    _note_uplinks :: [LinkContext]
  }
  deriving (Generic, ToJSON, FromJSON)

data LinkContext = LinkContext
  { _linkcontext_id :: EM.WikiLinkID,
    _linkcontext_label :: EM.WikiLinkLabel,
    _linkcontext_ctx :: Pandoc
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Ord LinkContext where
  compare = compare `on` _linkcontext_id

data LinkStatus
  = LinkStatus_Orphaned
  | LinkStatus_Connected
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data EmanoteApi :: * -> * where
  EmanoteApi_GetRev :: EmanoteApi Zk.Rev
  EmanoteApi_GetNotes :: EmanoteApi (Zk.Rev, [(LinkStatus, EM.WikiLinkID)])
  EmanoteApi_Note :: EM.WikiLinkID -> EmanoteApi (Zk.Rev, Note)

deriveGShow ''EmanoteApi
deriveJSONGADT ''EmanoteApi
deriveArgDict ''EmanoteApi

deriving instance Show (EmanoteApi Zk.Rev)

deriving instance Show (EmanoteApi [EM.WikiLinkID])

deriving instance Show (EmanoteApi (Maybe Zettel))

type EmanoteNet t m = RequesterT t EmanoteApi (Either Text) m
