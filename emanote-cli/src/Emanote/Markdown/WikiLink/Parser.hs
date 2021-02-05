{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module Emanote.Markdown.WikiLink.Parser
  ( wikiLinkSpec,
    WikiLinkID,
    WikiLinkLabel (..),
    WikiLinkContext,
    parseWikiLinkUrl,
    renderWikiLinkUrl,
    Directed (..),
    isParent,
    isBranch,
    isReverse,
  )
where

import qualified Commonmark as CM
import qualified Commonmark.Inlines as CM
import Commonmark.TokParsers (noneOfToks, symbol)
import Data.Tagged (Tagged (..), untag)
import Emanote.Markdown.WikiLink
import qualified Text.Megaparsec as M
import qualified Text.Parsec as P

wikiLinkSpec ::
  (Monad m, CM.IsBlock il bl, CM.IsInline il) =>
  CM.SyntaxSpec m il bl
wikiLinkSpec =
  mempty
    { CM.syntaxInlineParsers = [pLink]
    }
  where
    pLink ::
      (Monad m, CM.IsInline il) =>
      CM.InlineParser m il
    pLink =
      P.try $
        P.choice
          [ -- All neuron type links; not propagating link type for now.
            cmAutoLink WikiLinkLabel_Branch <$> P.try (wikiLinkP 3),
            cmAutoLink WikiLinkLabel_Tag <$> P.try (symbol '#' *> wikiLinkP 2),
            cmAutoLink WikiLinkLabel_Branch <$> P.try (wikiLinkP 2 <* symbol '#'),
            cmAutoLink WikiLinkLabel_Unlabelled <$> P.try (wikiLinkP 2)
          ]
    wikiLinkP :: Monad m => Int -> P.ParsecT [CM.Tok] s m WikiLinkID
    wikiLinkP n = do
      void $ M.count n $ symbol '['
      s <-
        fmap CM.untokenize $
          some $
            noneOfToks [CM.Symbol ']', CM.Symbol '[', CM.LineEnd]
      void $ M.count n $ symbol ']'
      pure $ Tagged s
    cmAutoLink :: CM.IsInline a => WikiLinkLabel -> WikiLinkID -> a
    cmAutoLink lbl iD =
      CM.link
        (renderWikiLinkUrl iD)
        (renderWikiLinkLabel lbl)
        (CM.str $ untag iD)
