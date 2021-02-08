{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Data.Conflict
  ( Conflict (..),
    resolveConflicts,
    lowerConflict,
    increaseConflict,
  )
where

import Data.Aeson
import qualified Data.List as List
import qualified Data.Map as Map
import Relude
import Relude.Extra (groupBy)

-- | Represent identifier conflicts
--
-- A conflict happens when a key (see @resolveConflicts@) maps to two or more
-- values, each identified by an unique identifier.
data Conflict identifier a = Conflict (identifier, a) (NonEmpty (identifier, a))
  deriving (Show, Generic, Eq)

-- TODO: clean up
instance ToJSON (Conflict FilePath ByteString) where
  toJSON (Conflict (k0, v0) rest) =
    let c' = (k0, decodeUtf8 @Text v0, fmap (second $ decodeUtf8 @Text) rest)
     in toJSON c'

instance FromJSON (Conflict FilePath ByteString) where
  parseJSON x = do
    (k0, v0, rest) <- parseJSON x
    pure $ Conflict (k0, encodeUtf8 @Text v0) $ fmap (second $ encodeUtf8 @Text) rest

resolveConflicts ::
  Ord k =>
  (identifier -> k) ->
  Map identifier v ->
  Map k (Either (Conflict identifier v) (identifier, v))
resolveConflicts toKey =
  Map.map
    ( \case
        (x :| []) -> Right x
        (x :| (y : ys)) -> Left $ Conflict x (y :| ys)
    )
    . groupBy (toKey . fst)
    . Map.toList

-- | Mark the given value as no longer conflicting.
--
-- If the conflict is resolved as a consequence, return the final value.
lowerConflict :: forall k a. Eq k => k -> Conflict k a -> Either (k, a) (Conflict k a)
lowerConflict x c = do
  case unconsConflict x c of
    Nothing ->
      Right c
    Just (a :| as) ->
      case nonEmpty as of
        Nothing -> Left a
        Just as' -> Right $ Conflict a as'

increaseConflict :: Eq identifier => identifier -> a -> Conflict identifier a -> Conflict identifier a
increaseConflict x v c =
  if x `identifierConflicts` c
    then c
    else consConflict x v c

unconsConflict :: Eq identifier => identifier -> Conflict identifier a -> Maybe (NonEmpty (identifier, a))
unconsConflict x (Conflict e es)
  | x == fst e = Just es
  | x `elem` fmap fst es = Just $ e :| List.filter ((== x) . fst) (toList es)
  | otherwise = Nothing

consConflict :: identifier -> a -> Conflict identifier a -> Conflict identifier a
consConflict x v (Conflict e1 (e2 :| es)) =
  Conflict (x, v) (e1 :| e2 : es)

identifierConflicts :: Eq identifier => identifier -> Conflict identifier a -> Bool
identifierConflicts x (Conflict e es) =
  x `elem` (fst <$> e : toList es)
