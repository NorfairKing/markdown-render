{-# LANGUAGE DeriveGeneric #-}

-- | The syntax tree of a document, and the functions that build one.
module Markdown.Render.Document
  ( -- * Building blocks
    Document (..),
    paragraph,
    heading,
    bulletList,
    orderedList,
    looseBulletList,
    looseOrderedList,
    codeBlock,
    blockQuote,
    thematicBreak,

    -- * Building inlines
    Inlines (..),
    text,
    code,
    emphasis,
    strong,
    link,
    image,
    hardBreak,

    -- * The syntax tree
    Block (..),
    Inline (..),
    HeadingLevel (..),
    ListSpacing (..),
    InfoString (..),
    Url (..),
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)

-- | A sequence of blocks.
--
-- A whole document and the contents of a list item or a block quote are all
-- one of these, so that they are built the same way and concatenated with
-- '<>'.
newtype Document = Document {documentBlocks :: [Block]}
  deriving (Show, Eq, Generic)

instance Validity Document

instance Semigroup Document where
  Document a <> Document b = Document (a <> b)

instance Monoid Document where
  mempty = Document []

-- | A sequence of inlines: the contents of a paragraph, a heading, emphasis or
-- a link label.
newtype Inlines = Inlines {inlinesList :: [Inline]}
  deriving (Show, Eq, Generic)

instance Validity Inlines

instance Semigroup Inlines where
  Inlines a <> Inlines b = Inlines (a <> b)

instance Monoid Inlines where
  mempty = Inlines []

-- | A string literal is text, so that a document can be written as
-- @paragraph (mconcat ["Status: ", strong "passed"])@.
instance IsString Inlines where
  fromString = text . Text.pack

data Block
  = BlockParagraph ![Inline]
  | BlockHeading !HeadingLevel ![Inline]
  | BlockBulletList !ListSpacing !(NonEmpty [Block])
  | BlockOrderedList !ListSpacing !(NonEmpty [Block])
  | BlockCodeBlock !(Maybe InfoString) !Text
  | BlockQuote ![Block]
  | BlockThematicBreak
  deriving (Show, Eq, Generic)

instance Validity Block

data Inline
  = -- | Rendered as exactly these characters.
    -- A newline is a line break in the source, which a reader joins up with
    -- the next line, as it would in a paragraph typed by hand.
    InlineText !Text
  | InlineCode !Text
  | InlineEmphasis ![Inline]
  | InlineStrong ![Inline]
  | InlineLink ![Inline] !Url
  | -- | The alt text, then where the image is.
    InlineImage ![Inline] !Url
  | InlineHardBreak
  deriving (Show, Eq, Generic)

instance Validity Inline

data HeadingLevel = H1 | H2 | H3 | H4 | H5 | H6
  deriving (Show, Eq, Generic)

instance Validity HeadingLevel

-- | Whether a list has a blank line between its items.
data ListSpacing = ListTight | ListLoose
  deriving (Show, Eq, Generic)

instance Validity ListSpacing

-- | What a code block says its contents are, usually a language like @bash@.
newtype InfoString = InfoString {unInfoString :: Text}
  deriving (Show, Eq, Generic)

instance Validity InfoString

newtype Url = Url {unUrl :: Text}
  deriving (Show, Eq, Generic)

instance Validity Url

paragraph :: Inlines -> Document
paragraph (Inlines is) = Document [BlockParagraph is]

heading :: HeadingLevel -> Inlines -> Document
heading level (Inlines is) = Document [BlockHeading level is]

-- | A list with one item per document, or nothing at all for no documents.
bulletList :: [Document] -> Document
bulletList = listOf (BlockBulletList ListTight)

orderedList :: [Document] -> Document
orderedList = listOf (BlockOrderedList ListTight)

looseBulletList :: [Document] -> Document
looseBulletList = listOf (BlockBulletList ListLoose)

looseOrderedList :: [Document] -> Document
looseOrderedList = listOf (BlockOrderedList ListLoose)

listOf :: (NonEmpty [Block] -> Block) -> [Document] -> Document
listOf makeList items = case NE.nonEmpty items of
  Nothing -> mempty
  Just ne -> Document [makeList (NE.map documentBlocks ne)]

codeBlock :: Maybe InfoString -> Text -> Document
codeBlock mInfo contents = Document [BlockCodeBlock mInfo contents]

blockQuote :: Document -> Document
blockQuote (Document bs) = Document [BlockQuote bs]

thematicBreak :: Document
thematicBreak = Document [BlockThematicBreak]

text :: Text -> Inlines
text t = Inlines [InlineText t]

code :: Text -> Inlines
code t = Inlines [InlineCode t]

emphasis :: Inlines -> Inlines
emphasis (Inlines is) = Inlines [InlineEmphasis is]

strong :: Inlines -> Inlines
strong (Inlines is) = Inlines [InlineStrong is]

link :: Inlines -> Url -> Inlines
link (Inlines is) url = Inlines [InlineLink is url]

-- | An image, with its alt text.
image :: Inlines -> Url -> Inlines
image (Inlines is) url = Inlines [InlineImage is url]

hardBreak :: Inlines
hardBreak = Inlines [InlineHardBreak]
