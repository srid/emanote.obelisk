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

data Note = Note
  { _note_wikiLinkID :: EM.WikiLinkID,
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
    -- | In the case of more than one link to the same note, this label
    -- represents the merged label (using Semigroup).
    _linkcontext_effectiveLabel :: EM.WikiLinkLabel,
    _linkcontext_ctxList :: [EM.WikiLinkContext]
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Ord LinkContext where
  compare = compare `on` _linkcontext_id

-- | Folgezettel affinity of a note
data Affinity
  = -- Has 1+ folgezettel parents
    Affinity_HasParents Natural
  | -- Has no folgezettel parent, but non-zero folgezettel children
    Affinity_Root
  | -- Has no folgezettel relation whatsoever
    Affinity_Orphaned
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data EmanoteState
  = -- | Not monitoring file changes
    EmanoteState_ReadOnly
  | -- | Moitoring filesystem, and at the given revision
    EmanoteState_AtRev Zk.Rev
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data EmanoteApi :: * -> * where
  EmanoteApi_GetRev :: EmanoteApi Zk.Rev
  EmanoteApi_GetNotes :: EmanoteApi (EmanoteState, [(Affinity, EM.WikiLinkID)])
  EmanoteApi_Note :: EM.WikiLinkID -> EmanoteApi (EmanoteState, Note)
  EmanoteApi_Search :: Text -> EmanoteApi [EM.WikiLinkID]

deriveGShow ''EmanoteApi
deriveJSONGADT ''EmanoteApi
deriveArgDict ''EmanoteApi

deriving instance Show (EmanoteApi Zk.Rev)

deriving instance Show (EmanoteApi [EM.WikiLinkID])

deriving instance Show (EmanoteApi (Maybe Zettel))

type EmanoteNet t m = RequesterT t EmanoteApi (Either Text) m
