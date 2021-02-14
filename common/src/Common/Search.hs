{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}

module Common.Search where

import Control.Applicative.Combinators.NonEmpty (sepBy1)
import qualified Data.Text as T
import Emanote.Markdown.WikiLink (WikiLinkID)
import Relude
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

data SearchQuery
  = SearchQuery_And (NonEmpty SearchQuery)
  | SearchQuery_Or (NonEmpty SearchQuery)
  | SearchQuery_TitleContains Text
  | SearchQuery_BranchesFrom WikiLinkID
  | SearchQuery_All
  deriving (Eq, Show)

titleContains :: Text -> SearchQuery
titleContains =
  SearchQuery_TitleContains

parseSearchQuery :: Text -> SearchQuery
parseSearchQuery (T.strip -> s) = do
  if T.null s
    then SearchQuery_All
    else parse (M.try searchQueryParser <|> pure (titleContains s)) s
  where
    parse p x =
      fromRight (titleContains s) $
        M.parse (p <* M.eof) "<search-query>" x

-- TODO: parse the rest (figure out tokens first, including quoted strings and :-separated tokens)
searchQueryParser :: M.Parsec Void Text SearchQuery
searchQueryParser = do
  -- TODO: Need to allow more than alpha num chars
  qs <- sepBy1 (toText <$> M.some M.alphaNumChar) M.space
  pure $ SearchQuery_And (titleContains <$> qs)
