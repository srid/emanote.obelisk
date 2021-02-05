{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Emanote.Markdown
  ( parseMarkdown,
    ParserError,
    MarkdownSyntaxSpec,
    markdownSpec,
  )
where

import qualified Commonmark as CM
import qualified Commonmark.Extensions as CE
import qualified Commonmark.Pandoc as CP
import Control.Monad.Combinators (manyTill)
import Data.Tagged (Tagged (..))
import Relude
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))
import qualified Text.Parsec as P

type Parser = FilePath -> Text -> Either ParserError Pandoc

type ParserError = Tagged "ParserError" Text

-- | Parse Markdown document, along with the YAML metadata block in it.
--
-- We are not using the Pandoc AST "metadata" field (as it is not clear whether
-- we actually need it), and instead directly decoding the metadata as Haskell
-- object.
parseMarkdown ::
  MarkdownSyntaxSpec m il bl =>
  CM.SyntaxSpec m il bl ->
  Parser
parseMarkdown extraSpec fn s = do
  (_metaVal, markdown) <-
    first (Tagged . ("Unable to determine YAML region: " <>)) $
      partitionMarkdown fn s
  v <-
    first (Tagged . show) $
      commonmarkPandocWith (extraSpec <> markdownSpec) fn markdown
  pure $ Pandoc mempty $ B.toList (CP.unCm v)

-- Like commonmarkWith, but parses directly into the Pandoc AST.
commonmarkPandocWith ::
  MarkdownSyntaxSpec m il bl =>
  CM.SyntaxSpec m il bl ->
  FilePath ->
  Text ->
  m bl
commonmarkPandocWith spec fn s =
  join $ CM.commonmarkWith spec fn s

-- | Identify metadata block at the top, and split it from markdown body.
partitionMarkdown :: FilePath -> Text -> Either Text (Maybe Text, Text)
partitionMarkdown =
  parse (M.try splitP <|> fmap (Nothing,) M.takeRest)
  where
    parse p fn s =
      first (toText . M.errorBundlePretty) $
        M.parse (p <* M.eof) fn s
    separatorP :: M.Parsec Void Text ()
    separatorP =
      void $ M.string "---" <* M.eol
    splitP :: M.Parsec Void Text (Maybe Text, Text)
    splitP = do
      separatorP
      a <- toText <$> manyTill M.anySingle (M.try $ M.eol *> separatorP)
      b <- M.takeRest
      pure (Just a, b)

-- | Like `MarkdownSyntaxSpec'` but specialized to Pandoc
type MarkdownSyntaxSpec m il bl =
  ( MarkdownSyntaxSpec' m il bl,
    m ~ Either P.ParseError,
    bl ~ CP.Cm () B.Blocks
  )

type MarkdownSyntaxSpec' m il bl =
  ( Monad m,
    CM.IsBlock il bl,
    CM.IsInline il,
    Typeable m,
    Typeable il,
    Typeable bl,
    CE.HasEmoji il,
    CE.HasStrikethrough il,
    CE.HasPipeTable il bl,
    CE.HasTaskList il bl,
    CM.ToPlainText il,
    CE.HasFootnote il bl,
    CE.HasMath il,
    CE.HasDefinitionList il bl,
    CE.HasDiv bl,
    CE.HasQuoted il,
    CE.HasSpan il
  )

markdownSpec ::
  MarkdownSyntaxSpec' m il bl =>
  CM.SyntaxSpec m il bl
markdownSpec =
  -- TODO: Move the bulk of markdown extensions to a neuron extension
  mconcat
    [ gfmExtensionsSansEmoji,
      CE.fancyListSpec,
      CE.footnoteSpec,
      CE.mathSpec,
      CE.smartPunctuationSpec,
      CE.definitionListSpec,
      CE.attributesSpec,
      CE.rawAttributeSpec,
      CE.fencedDivSpec,
      CE.bracketedSpanSpec,
      CE.autolinkSpec,
      CM.defaultSyntaxSpec,
      -- as the commonmark documentation states, pipeTableSpec should be placed after
      -- fancyListSpec and defaultSyntaxSpec to avoid bad results when non-table lines
      CE.pipeTableSpec
    ]
  where
    -- Emoji extension introduces ghcjs linker issues
    gfmExtensionsSansEmoji =
      CE.strikethroughSpec
        <> CE.autoIdentifiersSpec
        <> CE.taskListSpec
