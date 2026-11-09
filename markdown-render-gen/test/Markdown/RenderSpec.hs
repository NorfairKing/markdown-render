{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Markdown.RenderSpec (spec) where

import qualified CMark
import Control.Monad (forM_)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import Data.Char (isSpace)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import Markdown.Render
import Markdown.Render.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @Document
  genValidSpec @Inlines
  genValidSpec @Block
  genValidSpec @Inline
  genValidSpec @HeadingLevel
  genValidSpec @ListSpacing
  genValidSpec @InfoString
  genValidSpec @Url

  describe "Document" $
    monoidSpec @Document

  describe "Inlines" $
    monoidSpec @Inlines

  describe "renderText" $ do
    describe "golden" $ do
      -- Each case pins both the rendering and the tree a reader gets back,
      -- because some of the tree, like where an ordered list starts, does not
      -- show in the rendering.
      let cases :: [(String, Document)]
          cases =
            [ ("escapes-markup-in-text", paragraph (text "# not a *heading* [x](y) `z` <b> &amp;")),
              ("leaves-out-escapes-a-reader-does-not-need", paragraph (text "[success] x86_64 a -> b, 2 * 3 = 6, #1 + #2, 50%! (yes)")),
              ("keeps-escapes-that-only-a-list-item-needs", bulletList [paragraph "--", paragraph "- -"] <> orderedList [paragraph "**"]),
              ("escapes-tildes-that-would-open-a-code-block", paragraph (mconcat ["~~~", hardBreak, "~~~ a\n~b ~~~"])),
              ("keeps-a-multi-line-message-inside-its-list-item", bulletList [paragraph (mconcat ["Message: ", text "fix it\n# Not a heading\n- not an item"])]),
              ("replaces-the-characters-libcmark-cannot-write", paragraph (text "a\NUL\xFFFE\xFFFF\&b")),
              ("keeps-bullet-lists-in-a-row-apart", mconcat [bulletList [paragraph "a"], bulletList [paragraph "b"], bulletList [paragraph "c"]]),
              ("keeps-ordered-lists-in-a-row-apart", mconcat [orderedList [paragraph "a"], orderedList [paragraph "b"], orderedList [paragraph "c"]]),
              ("writes-a-thematic-break-that-cannot-underline-the-line-above-it", bulletList [paragraph "a" <> thematicBreak]),
              ("loosens-a-tight-list-whose-paragraphs-would-run-together", bulletList [paragraph "a" <> paragraph "b"]),
              ("loosens-a-tight-list-whose-quotes-would-run-together", bulletList [blockQuote (paragraph "a") <> blockQuote (paragraph "b")]),
              ("keeps-a-tight-list-whose-quote-follows-a-paragraph", bulletList [paragraph "a" <> blockQuote (paragraph "b")]),
              ("loosens-a-tight-list-whose-paragraph-would-continue-a-nested-list", bulletList [bulletList [paragraph "a"] <> paragraph "b"]),
              ("loosens-a-tight-list-whose-paragraph-would-continue-a-quote", bulletList [blockQuote (paragraph "a") <> paragraph "b"]),
              ("keeps-a-tight-list-whose-paragraph-follows-a-nested-thematic-break", bulletList [bulletList [thematicBreak] <> paragraph "b"]),
              ("writes-tight-lists", bulletList [paragraph "a", paragraph "b"] <> orderedList [paragraph "c", paragraph "d"]),
              ("separates-the-items-of-a-loose-list", looseBulletList [paragraph "a", paragraph "b"] <> looseOrderedList [paragraph "c", paragraph "d"]),
              ("keeps-a-loose-list-with-nothing-to-separate-tight", looseBulletList [paragraph "a"]),
              ("keeps-a-loose-list-separated-only-after-thematic-breaks-tight", looseBulletList [thematicBreak, bulletList [thematicBreak], paragraph "a"]),
              ("keeps-a-loose-list-separated-after-a-quoted-thematic-break-loose", looseBulletList [blockQuote thematicBreak, paragraph "a"]),
              ("leaves-out-empty-list-items", bulletList [mempty, paragraph "a", mempty]),
              ("leaves-out-a-list-of-only-empty-items", paragraph "a" <> bulletList [mempty, paragraph mempty] <> paragraph "b"),
              ("writes-every-heading-level", mconcat [heading H1 "a", heading H2 "b", heading H3 "c", heading H4 "d", heading H5 "e", heading H6 "f"]),
              ("puts-a-heading-on-one-line", heading H2 (mconcat ["a\nb", hardBreak, "c", link (emphasis "d\ne") (Url "f")])),
              ("writes-an-empty-quote", blockQuote mempty),
              ("separates-the-blocks-in-a-quote", blockQuote (paragraph "a" <> paragraph "b")),
              ("escapes-the-info-string-of-a-code-block", codeBlock (Just (InfoString " a\\-b\n\t")) "x"),
              ("fences-a-code-block-with-tildes-when-its-info-string-has-a-backtick", codeBlock (Just (InfoString "a`b")) "```\n~~~"),
              ("fences-a-code-block-with-more-backticks-than-it-contains", codeBlock (Just (InfoString "sh")) "````\n"),
              ("ends-every-line-of-a-code-block-with-a-newline", codeBlock Nothing "a\r\nb\rc"),
              ("writes-an-empty-code-block", codeBlock Nothing ""),
              ("puts-a-code-span-on-one-line", paragraph (code "a ` b\nc\r\nd\re")),
              ("joins-code-spans-in-a-row", paragraph (mconcat [code "a", code "b"])),
              ("leaves-out-an-empty-code-span", paragraph (mconcat ["a", code "", "b"])),
              ("trims-a-paragraph", paragraph (mconcat [hardBreak, "  ", code "a", " b  ", "\n"])),
              ("leaves-out-a-paragraph-of-only-whitespace", paragraph "a" <> paragraph (mconcat [" ", hardBreak, "\n "]) <> paragraph "b"),
              ("strips-spaces-around-line-breaks", paragraph (mconcat ["a  \n  b  ", hardBreak, "  c"])),
              ("makes-breaks-in-a-row-into-one", paragraph (mconcat ["a", hardBreak, hardBreak, "b\n\nc\n", hardBreak, "d"])),
              ("moves-whitespace-out-of-emphasis", paragraph (mconcat ["a", strong " b ", "c", emphasis "\nd\n", "e", strong (mconcat [hardBreak, "f"])])),
              ("leaves-out-emphasis-of-only-whitespace", paragraph (mconcat ["a", strong " ", emphasis (mconcat [" ", hardBreak, " "]), "b"])),
              ("drops-emphasis-that-a-reader-would-not-see-as-emphasis", paragraph (mconcat ["a", strong "(b)", "c ", strong "(d)", "e ", strong "\x20AC\&f", " g", strong "h\x20AC", "\x20AC"])),
              ("drops-emphasis-that-starts-with-punctuation-after-a-letter", paragraph (mconcat ["a", strong (mconcat [code "b", "c"]), " d", emphasis (mconcat [code "e", "f"])])),
              ("keeps-emphasis-next-to-punctuation-and-whitespace", paragraph (mconcat ["a ", strong "(b)", " c ", emphasis "(d)", ".", code "e", strong "(f)", code "g"])),
              ("writes-emphasis-of-strong-emphasis", paragraph (strong (emphasis "a"))),
              ("unwraps-emphasis-inside-emphasis", paragraph (emphasis (mconcat ["a ", emphasis "b", " c"])) <> paragraph (strong (mconcat ["d ", strong "e", " f"]))),
              ("unwraps-emphasis-at-the-edge-of-emphasis", paragraph (mconcat [strong (mconcat [emphasis "a", " b"]), " ", strong (mconcat ["c ", emphasis "d"])])),
              ("joins-emphasis-in-a-row", paragraph (mconcat [emphasis "a", emphasis "b", " ", strong "c", strong "d"])),
              ("unwraps-the-second-of-two-emphases-in-a-row", paragraph (mconcat [emphasis "a", strong "b"])),
              ("checks-emphasis-inside-links-and-images", paragraph (mconcat [link (mconcat ["a", strong "(b)", "c"]) (Url "d"), image (strong "(e)") (Url "f")])),
              ("keeps-emphasis-next-to-links-images-and-breaks", paragraph (mconcat ["a ", strong "(b)", link "c" (Url "d"), " e ", strong "(f)", image "g" (Url "h"), " ", strong "(i)", "\nj ", strong "(k)", hardBreak, "l ", strong (link "m" (Url "n")), " ", emphasis (image "o" (Url "p"))])),
              ("keeps-links-inside-emphasis", paragraph (mconcat [emphasis (mconcat ["a ", link "b\nc" (Url "d")]), " ", strong (mconcat ["e ", link "f" (Url "g")])])),
              ("keeps-the-text-of-links-inside-links-inside-links", paragraph (link (link (mconcat ["a ", link "b\nc" (Url "d")]) (Url "e")) (Url "f"))),
              ("keeps-a-link-inside-an-image", paragraph (image (mconcat ["a ", link "b\nc" (Url "d")]) (Url "e"))),
              ("writes-an-image-with-its-alt-text", paragraph (image (mconcat ["a ", emphasis "b"]) (Url "c.png"))),
              ("keeps-the-text-of-a-link-inside-a-link", paragraph (link (mconcat ["a ", link "b" (Url "inner")]) (Url "outer"))),
              ("trims-breaks-off-the-edges-of-a-label", paragraph (link (mconcat [hardBreak, "a\nb", "\n"]) (Url "c"))),
              ("does-not-write-a-link-that-would-read-as-a-link-reference-definition", paragraph (mconcat [" ", link (mconcat [emphasis "a", code "]:"]) (Url "u"), strong "b"])),
              ("writes-a-link-with-brackets-in-its-label-elsewhere", paragraph (mconcat ["a ", link (code "]:") (Url "u")])),
              ("percent-encodes-whitespace-in-a-link-destination", paragraph (link "a" (Url "http://x/a b\tc\nd\ve\ff\rg\xFFFF\&h"))),
              ( "renders-a-build-report",
                mconcat
                  [ heading H1 "main: Fix the cache key",
                    bulletList
                      [ paragraph (mconcat [strong "Commit", ": ", code "1a2b3c4"]),
                        paragraph (mconcat [strong "Message", ": ", text "Fix the cache key\n\n* It was [wrong](no)\n# for `nix` 2.18"]),
                        paragraph (mconcat [strong "Author", ": ", text "Jane Doe <jane@example.com>"])
                      ],
                    heading H2 "Runs",
                    bulletList
                      [ paragraph (mconcat ["[success] ", link "build packages.x86_64-linux.default" (Url "https://ci.example.com/run/1")]),
                        paragraph (mconcat ["[failure] ", link "test checks.x86_64-linux.e2e_test" (Url "https://ci.example.com/run/2"), " (attempt 2)"])
                          <> bulletList [paragraph (mconcat ["Dependencies: ", code "packages.x86_64-linux.default"])]
                      ],
                    heading H2 "Reproduce",
                    codeBlock (Just (InfoString "bash")) "nix build .#checks.x86_64-linux.e2e_test\n",
                    blockQuote (paragraph (mconcat ["Logs are at ", link "the run page" (Url "https://ci.example.com/run/2"), "."]))
                  ]
              )
            ]
      forM_ cases $ \(name, document) -> describe name $ do
        it "renders the same as before" $
          pureGoldenTextFile (concat ["test_resources/rendering/", name, ".md"]) (renderText document)
        it "is read back as the same tree as before" $
          goldenPrettyShowInstance (concat ["test_resources/tree/", name, ".txt"]) (documentToNode document)
        it "is read back by libcmark as that tree" $ do
          let stripPositions :: CMark.Node -> CMark.Node
              stripPositions (CMark.Node _ typ children) =
                CMark.Node Nothing typ (map stripPositions (filter (\(CMark.Node _ t _) -> t /= CMark.TEXT "") children))
          stripPositions (CMark.commonmarkToNode [CMark.optNormalize] (renderText document))
            `shouldBe` documentToNode document

    modifyMaxSuccess (const 1000) $
      it "is read back by libcmark as documentToNode" $
        forAllValid $ \document -> do
          let stripPositions :: CMark.Node -> CMark.Node
              stripPositions (CMark.Node _ typ children) =
                CMark.Node Nothing typ (map stripPositions (filter (not . isEmptyText) children))
              -- libcmark's parser leaves an empty text node behind where it takes
              -- the spaces of a hard break off the end of something other than
              -- text.
              isEmptyText :: CMark.Node -> Bool
              isEmptyText (CMark.Node _ typ _) = typ == CMark.TEXT ""
          let rendered = renderText document
          context (unlines [show document, Text.unpack rendered]) $
            stripPositions (CMark.commonmarkToNode [CMark.optNormalize] rendered)
              `shouldBe` documentToNode document

  describe "documentToNode" $
    modifyMaxSuccess (const 1000) $
      it "keeps every character other than whitespace, in order" $
        forAllValid $ \document -> do
          let visible :: Text.Text -> String
              visible =
                map (\c -> if c `elem` ("\NUL\xFFFE\xFFFF" :: String) then '\xFFFD' else c)
                  . filter (not . isSpace)
                  . Text.unpack
          let blockText :: Block -> String
              blockText = \case
                BlockParagraph is -> concatMap inlineText is
                BlockHeading _ is -> concatMap inlineText is
                BlockBulletList _ items -> concatMap (concatMap blockText) items
                BlockOrderedList _ items -> concatMap (concatMap blockText) items
                BlockCodeBlock mInfo contents -> concat [maybe "" (visible . unInfoString) mInfo, visible contents]
                BlockQuote bs -> concatMap blockText bs
                BlockThematicBreak -> ""
              inlineText :: Inline -> String
              inlineText = \case
                InlineText t -> visible t
                InlineCode t -> visible t
                InlineEmphasis is -> concatMap inlineText is
                InlineStrong is -> concatMap inlineText is
                InlineLink is _ -> concatMap inlineText is
                InlineImage is _ -> concatMap inlineText is
                InlineHardBreak -> ""
          let nodeText :: CMark.Node -> String
              nodeText (CMark.Node _ typ children) = case typ of
                CMark.TEXT t -> visible t
                CMark.CODE t -> visible t
                CMark.CODE_BLOCK info contents -> concat [visible info, visible contents]
                _ -> concatMap nodeText children
          nodeText (documentToNode document) `shouldBe` concatMap blockText (documentBlocks document)

  describe "renderLazyText" $
    it "renders the same text" $
      forAllValid $ \document ->
        LT.toStrict (renderLazyText document) `shouldBe` renderText document

  describe "renderTextBuilder" $
    it "renders the same text" $
      forAllValid $ \document ->
        LT.toStrict (TB.toLazyText (renderTextBuilder document)) `shouldBe` renderText document

  describe "renderByteString" $
    it "renders the same text in UTF-8" $
      forAllValid $ \document ->
        renderByteString document `shouldBe` TE.encodeUtf8 (renderText document)

  describe "renderLazyByteString" $
    it "renders the same text in UTF-8" $
      forAllValid $ \document ->
        LB.toStrict (renderLazyByteString document) `shouldBe` TE.encodeUtf8 (renderText document)

  describe "renderByteStringBuilder" $
    it "renders the same text in UTF-8" $
      forAllValid $ \document ->
        LB.toStrict (BB.toLazyByteString (renderByteStringBuilder document)) `shouldBe` TE.encodeUtf8 (renderText document)

  describe "renderString" $
    it "renders the same text" $
      forAllValid $ \document ->
        Text.pack (renderString document) `shouldBe` renderText document
