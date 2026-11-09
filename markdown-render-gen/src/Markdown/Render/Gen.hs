{-# OPTIONS_GHC -Wno-orphans #-}

module Markdown.Render.Gen (genMarkdownText) where

import Data.GenValidity
import Data.GenValidity.Text ()
import Data.Text (Text)
import qualified Data.Text as Text
import Markdown.Render
import Test.QuickCheck

instance GenValid Document

instance GenValid Inlines

instance GenValid Block where
  genValid = sized $ \size ->
    let leaves =
          [ BlockParagraph <$> genListOf genValid,
            BlockHeading <$> genValid <*> genListOf genValid,
            BlockCodeBlock <$> genValid <*> genMarkdownText,
            pure BlockThematicBreak
          ]
        containers =
          [ BlockBulletList <$> genValid <*> genNonEmptyOf (genListOf genValid),
            BlockOrderedList <$> genValid <*> genNonEmptyOf (genListOf genValid),
            BlockQuote <$> genListOf genValid
          ]
     in oneof (if size < 2 then leaves else leaves ++ containers)

instance GenValid Inline where
  genValid = sized $ \size ->
    let leaves =
          [ InlineText <$> genMarkdownText,
            InlineCode <$> genMarkdownText,
            pure InlineHardBreak
          ]
        containers =
          [ InlineEmphasis <$> genListOf genValid,
            InlineStrong <$> genListOf genValid,
            InlineLink <$> genListOf genValid <*> genValid,
            InlineImage <$> genListOf genValid <*> genValid
          ]
     in oneof (if size < 2 then leaves else leaves ++ containers)

instance GenValid HeadingLevel

instance GenValid ListSpacing

instance GenValid InfoString where
  genValid = InfoString <$> genMarkdownText

instance GenValid Url where
  genValid = Url <$> genMarkdownText

-- | Any text, but often made of the characters that mean something in
-- markdown, which arbitrary text hardly ever puts next to each other.
genMarkdownText :: Gen Text
genMarkdownText =
  oneof
    [ genValid,
      Text.pack <$> listOf (elements "*_`[]()<>!#-+=.:1aZ \t\n\r\\&|~\"'")
    ]
