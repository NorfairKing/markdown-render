-- | Build a CommonMark document as a value and render it.
--
-- Every piece of text that goes into a document is text, never markup: it
-- renders as those characters, whatever they are.
-- libcmark, the reference implementation of CommonMark, escapes the text in
-- each block, this library leaves out the escapes that a reader does not need,
-- and lays the blocks out.
--
-- Some documents cannot be written down exactly, like emphasis that starts
-- with a space, or a list item with nothing in it.
-- Those render as the closest document that can, which is the one
-- 'documentToNode' gives, and which a reader parses back out of the rendering.
-- What gets lost on the way is whitespace and markup, never other text, except
-- for the few characters that cannot be written at all, which become the
-- replacement character.
module Markdown.Render
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

    -- * Rendering
    renderText,
    renderLazyText,
    renderTextBuilder,
    renderByteString,
    renderLazyByteString,
    renderByteStringBuilder,
    renderString,
    renderChunks,

    -- * cmark
    documentToNode,
  )
where

import qualified CMark
import qualified Data.ByteString as SB
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (foldMap')
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import Markdown.Render.Document
import Markdown.Render.Layout
import Markdown.Render.Node

renderText :: Document -> Text
renderText = Text.concat . renderChunks

-- | Built from 'renderChunks', so that it is produced as it is consumed.
renderLazyText :: Document -> LT.Text
renderLazyText = LT.fromChunks . renderChunks

renderTextBuilder :: Document -> TB.Builder
renderTextBuilder = foldMap' TB.fromText . renderChunks

renderByteString :: Document -> SB.ByteString
renderByteString = TE.encodeUtf8 . renderText

-- | Built from 'renderChunks', so that it is produced as it is consumed.
renderLazyByteString :: Document -> LB.ByteString
renderLazyByteString = LB.fromChunks . map TE.encodeUtf8 . renderChunks

renderByteStringBuilder :: Document -> BB.Builder
renderByteStringBuilder = foldMap' TE.encodeUtf8Builder . renderChunks

renderString :: Document -> String
renderString = Text.unpack . renderText

-- | The rendering, one top-level block at a time.
--
-- The chunks are produced lazily, so this is what to stream a large document
-- from.
renderChunks :: Document -> [Text]
renderChunks document =
  let CMark.Node _ _ blocks = documentToNode document
   in blockChunks blocks
