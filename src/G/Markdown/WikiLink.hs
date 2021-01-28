{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module G.Markdown.WikiLink
  ( wikiLinkSpec,
    ID,
  )
where

import qualified Commonmark as CM
import qualified Commonmark.Inlines as CM
import Commonmark.TokParsers (noneOfToks, symbol)
import Data.Tagged (Tagged)
import qualified Data.Text as T
import qualified Text.Megaparsec as M
import qualified Text.Parsec as P

-- | ID of a Markdown file. Semantically the same as wiki-link's body.
type ID = Tagged "ID" Text

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
            cmAutoLink <$> P.try (wikiLinkP 3),
            cmAutoLink <$> P.try (symbol '#' *> wikiLinkP 2),
            cmAutoLink <$> P.try (wikiLinkP 2 <* symbol '#'),
            cmAutoLink <$> P.try (wikiLinkP 2)
          ]
    wikiLinkP :: Monad m => Int -> P.ParsecT [CM.Tok] s m Text
    wikiLinkP n = do
      void $ M.count n $ symbol '['
      s <- fmap CM.untokenize $ some $ noneOfToks [CM.Symbol ']', CM.Symbol '[', CM.LineEnd]
      void $ M.count n $ symbol ']'
      pure s
    cmAutoLink :: CM.IsInline a => Text -> a
    cmAutoLink url =
      let htmlUrl = (T.replace " " "-" . T.toLower) url <> ".html" -- HACK, until we do it properly
       in CM.link htmlUrl "" $ CM.str url
