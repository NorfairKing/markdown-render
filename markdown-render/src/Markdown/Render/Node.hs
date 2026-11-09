{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The tree that a reader parses back out of the rendering of a document.
--
-- [check:file] Mutation testing is off for this module, because the golden
-- tests of the rendering pin every line of it down.
-- Add a golden test for any new case.
module Markdown.Render.Node
  ( documentToNode,
    asciiPunctuation,
    node,
  )
where

import qualified CMark
import Data.Char (isAlphaNum, isSpace)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as Text
import Markdown.Render.Document

{-# ANN module ("DisableMutations" :: String) #-}

-- | The tree that a reader parses back out of the rendering.
documentToNode :: Document -> CMark.Node
documentToNode (Document blocks) =
  CMark.Node Nothing CMark.DOCUMENT (blocksToNodes blocks)

-- | A sequence of blocks, in which an ordered list right after another one has
-- the other delimiter, so that a reader does not read them as one list.
blocksToNodes :: [Block] -> [CMark.Node]
blocksToNodes =
  let go :: CMark.DelimType -> [CMark.Node] -> [CMark.Node]
      go delimiter = \case
        [] -> []
        (CMark.Node p (CMark.LIST attributes) items : rest)
          | CMark.listType attributes == CMark.ORDERED_LIST ->
              CMark.Node p (CMark.LIST attributes {CMark.listDelim = delimiter}) items
                : go (otherDelimiter delimiter) rest
        (b : rest) -> b : go CMark.PERIOD_DELIM rest
      otherDelimiter :: CMark.DelimType -> CMark.DelimType
      otherDelimiter = \case
        CMark.PERIOD_DELIM -> CMark.PAREN_DELIM
        CMark.PAREN_DELIM -> CMark.PERIOD_DELIM
   in go CMark.PERIOD_DELIM . concatMap blockToNodes

blockToNodes :: Block -> [CMark.Node]
blockToNodes = \case
  BlockParagraph is -> case paragraphInlines is of
    -- A paragraph with nothing in it renders as nothing at all.
    [] -> []
    ns -> [node CMark.PARAGRAPH ns]
  BlockHeading level is -> [node (CMark.HEADING (headingLevelNumber level)) (headingInlines is)]
  BlockBulletList spacing items -> listNodes CMark.BULLET_LIST spacing items
  BlockOrderedList spacing items -> listNodes CMark.ORDERED_LIST spacing items
  BlockCodeBlock mInfo contents ->
    -- A code block is made of whole lines, so what a reader gets back out of
    -- one ends in a newline whether or not what went in did, and every line
    -- ending in it is a newline.
    let newlines = Text.replace "\r" "\n" (Text.replace "\r\n" "\n" (replaceUnwritable contents))
        lineContents =
          if Text.null newlines || "\n" `Text.isSuffixOf` newlines
            then newlines
            else Text.snoc newlines '\n'
        -- The info string is what is on the line after the fence, without
        -- the ASCII whitespace around it.
        info = maybe "" (Text.dropAround (`elem` (" \t\v\f" :: String)) . oneLine . replaceUnwritable . unInfoString) mInfo
     in [node (CMark.CODE_BLOCK info lineContents) []]
  BlockQuote bs -> [node CMark.BLOCK_QUOTE (blocksToNodes bs)]
  BlockThematicBreak -> [node CMark.THEMATIC_BREAK []]

-- | A list of the items that have something in them, or nothing at all if none
-- do.
--
-- An empty item says nothing, and a reader takes one right under a line of
-- text to underline it as a heading instead.
listNodes :: CMark.ListType -> ListSpacing -> NonEmpty [Block] -> [CMark.Node]
listNodes listType spacing items =
  let itemNodes = filter (not . null) $ map blocksToNodes (NE.toList items)
      attributes =
        CMark.ListAttributes
          { CMark.listType = listType,
            CMark.listTight = listIsTight spacing itemNodes,
            -- A bullet list has no start, which libcmark reads back as zero.
            CMark.listStart = case listType of
              CMark.BULLET_LIST -> 0
              CMark.ORDERED_LIST -> 1,
            CMark.listDelim = CMark.PERIOD_DELIM
          }
   in [node (CMark.LIST attributes) (map (node CMark.ITEM) itemNodes) | not (null itemNodes)]

-- | Whether a reader sees a list as tight.
--
-- A list is loose when a blank line separates two of its items, or two blocks
-- in one of its items, but libcmark does not count a blank line right after a
-- thematic break.
-- Where nothing that counts separates anything, a loose list reads as a
-- tight one.
-- Where two blocks in an item would run into each other without a blank line
-- between them, a tight list has to be written as a loose one.
listIsTight :: ListSpacing -> [[CMark.Node]] -> Bool
listIsTight spacing itemNodes =
  let allButLast :: [a] -> [a]
      allButLast = reverse . drop 1 . reverse
      itemFitsTight :: [CMark.Node] -> Bool
      itemFitsTight blocks = and (zipWith mayFollowDirectly blocks (drop 1 blocks))
      separationCounts :: CMark.Node -> Bool
      separationCounts = not . endsWithThematicBreak
      separationsCount =
        any (any separationCounts . lastOf) (allButLast itemNodes)
          || any (any separationCounts . allButLast) itemNodes
   in case spacing of
        ListTight | all itemFitsTight itemNodes -> True
        _ -> not separationsCount

-- | Whether the second block reads as itself when it starts on the line right
-- after the first, rather than continuing it.
mayFollowDirectly :: CMark.Node -> CMark.Node -> Bool
mayFollowDirectly before (CMark.Node _ after _) =
  let isParagraph :: CMark.NodeType -> Bool
      isParagraph = \case
        CMark.PARAGRAPH -> True
        _ -> False
      isBlockQuote :: CMark.Node -> Bool
      isBlockQuote (CMark.Node _ typ _) = case typ of
        CMark.BLOCK_QUOTE -> True
        _ -> False
   in case after of
        -- Text would continue a paragraph that is still open.
        CMark.PARAGRAPH -> not (endsWith isParagraph before)
        -- A quote would continue the quote.
        CMark.BLOCK_QUOTE -> not (isBlockQuote before)
        _ -> True

-- | Whether this block, or the last block in it if it is a list, wherever that
-- is, is a thematic break.
--
-- libcmark does not look inside a quote for one.
endsWithThematicBreak :: CMark.Node -> Bool
endsWithThematicBreak (CMark.Node _ typ children) = case typ of
  CMark.LIST _ -> any (\(CMark.Node _ _ itemBlocks) -> any endsWithThematicBreak (lastOf itemBlocks)) (lastOf children)
  CMark.THEMATIC_BREAK -> True
  _ -> False

-- | Whether this block, or the last block in it if it is a list or a quote,
-- wherever that is, is one of these.
endsWith :: (CMark.NodeType -> Bool) -> CMark.Node -> Bool
endsWith p (CMark.Node _ typ children) = case typ of
  CMark.LIST _ -> any (\(CMark.Node _ _ itemBlocks) -> any (endsWith p) (lastOf itemBlocks)) (lastOf children)
  CMark.BLOCK_QUOTE -> any (endsWith p) (lastOf children)
  _ -> p typ

-- | The last of these, if there is one.
lastOf :: [a] -> [a]
lastOf = take 1 . reverse

headingLevelNumber :: HeadingLevel -> Int
headingLevelNumber = \case
  H1 -> 1
  H2 -> 2
  H3 -> 3
  H4 -> 4
  H5 -> 5
  H6 -> 6

-- | Whether a reader can see a line break where these inlines go.
--
-- A heading is one line, so a break in one renders as the space a reader would
-- see in its place.
data Breaks = BreaksKept | BreaksAsSpaces

-- | Whether these inlines are already inside a link, which CommonMark does not
-- let hold another link.
data LinkNesting = OutsideLink | InsideLink

-- | The contents of a paragraph, without what a reader drops from its edges:
-- spaces, and breaks with nothing on the other side of them.
--
-- A paragraph that starts with what looks like a link label followed by a
-- colon is a link reference definition instead, which a link whose label has
-- a code span with that in it would look like.
-- Such a link is replaced by its label.
paragraphInlines :: [Inline] -> [CMark.Node]
paragraphInlines is =
  let looksLikeDefinition :: [CMark.Node] -> Bool
      looksLikeDefinition = any $ \(CMark.Node _ typ children) -> case typ of
        CMark.CODE t -> "]:" `Text.isInfixOf` t
        _ -> looksLikeDefinition children
   in case trimEdges (inlinesToNodes BreaksKept OutsideLink is) of
        (CMark.Node _ (CMark.LINK _ _) label : rest)
          | looksLikeDefinition label ->
              keepReadableEmphasis (trimEdges (mergeTexts (label ++ rest)))
        ns -> keepReadableEmphasis ns

-- | The contents of a heading.
headingInlines :: [Inline] -> [CMark.Node]
headingInlines = keepReadableEmphasis . trimEdges . inlinesToNodes BreaksAsSpaces OutsideLink

-- | The contents of a link label or an image's alt text, without breaks at its
-- edges, which a reader does not see as breaks.
labelInlines :: Breaks -> LinkNesting -> [Inline] -> [CMark.Node]
labelInlines breaks nesting =
  reverse . dropWhile isBreak . reverse . dropWhile isBreak . inlinesToNodes breaks nesting

inlinesToNodes :: Breaks -> LinkNesting -> [Inline] -> [CMark.Node]
inlinesToNodes breaks nesting =
  collapseBreaks
    . dropEmptyTexts
    . stripSpacesAroundBreaks
    . mergeTexts
    . concatMap (inlineToNodes breaks nesting)

inlineToNodes :: Breaks -> LinkNesting -> Inline -> [CMark.Node]
inlineToNodes breaks nesting = \case
  InlineText t ->
    let lineBreak = case breaks of
          BreaksKept -> node CMark.SOFTBREAK []
          BreaksAsSpaces -> textNode " "
     in NE.toList $
          NE.intersperse lineBreak $
            NE.map textNode (textLines (replaceUnwritable t))
  InlineCode t ->
    -- A reader sees each line ending in a code span as a space, and there is
    -- no way to write a code span with nothing in it.
    let contents = oneLine (replaceUnwritable t)
     in [node (CMark.CODE contents) [] | not (Text.null contents)]
  InlineEmphasis is -> delimited CMark.EMPH (inlinesToNodes breaks nesting is)
  InlineStrong is -> delimited CMark.STRONG (inlinesToNodes breaks nesting is)
  InlineLink is url -> case nesting of
    InsideLink -> inlinesToNodes breaks nesting is
    OutsideLink -> [node (CMark.LINK (urlDestination url) "") (labelInlines breaks InsideLink is)]
  InlineImage is url -> [node (CMark.IMAGE (urlDestination url) "") (labelInlines breaks nesting is)]
  InlineHardBreak -> case breaks of
    BreaksKept -> [node CMark.LINEBREAK []]
    BreaksAsSpaces -> [textNode " "]

-- | These inlines with every emphasis of the given kind in them replaced by
-- what is inside it.
--
-- Emphasis inside emphasis of the same kind reads as no more emphasised, and
-- writing it would take a run of delimiters that a reader splits up
-- differently.
unwrapAll :: CMark.NodeType -> [CMark.Node] -> [CMark.Node]
unwrapAll typ =
  let go :: CMark.Node -> [CMark.Node]
      go (CMark.Node p t cs) =
        if t == typ
          then unwrapAll typ cs
          else [CMark.Node p t (unwrapAll typ cs)]
   in mergeTexts . concatMap go

-- | What the character on one side of an emphasis delimiter is, as far as
-- deciding whether the delimiter opens or closes emphasis goes.
data EdgeCharacter
  = -- | Whitespace, punctuation, or the start or end of a line.
    EdgeSpaceOrPunctuation
  | EdgeAlphanumeric
  | -- | Anything else, which emphasis is kept away from to be safe.
    EdgeUnknown

-- | These inlines with every emphasis that a reader would not see as emphasis
-- replaced by what is inside it.
--
-- A delimiter opens emphasis when what follows it is alphanumeric, or when
-- what follows it is punctuation and what precedes it is whitespace or
-- punctuation, and the other way around to close it.
-- Where neither holds, there is no way to write the emphasis, and its text is
-- what is left to keep.
-- Dropping one emphasis changes what the delimiters next to it are next to,
-- so this repeats until nothing changes.
keepReadableEmphasis :: [CMark.Node] -> [CMark.Node]
keepReadableEmphasis ns =
  let ns' = emphasisPass EdgeSpaceOrPunctuation EdgeSpaceOrPunctuation ns
   in if ns' == ns then ns else keepReadableEmphasis ns'

emphasisPass :: EdgeCharacter -> EdgeCharacter -> [CMark.Node] -> [CMark.Node]
emphasisPass outerLeft outerRight unseparated =
  let nodes = map strongOutside (separateEmphasis unseparated)
      lefts = outerLeft : map lastEdge nodes
      rights = drop 1 (map firstEdge nodes) ++ [outerRight]
      -- Delimiters and brackets are punctuation.
      inside :: [CMark.Node] -> [CMark.Node]
      inside = emphasisPass EdgeSpaceOrPunctuation EdgeSpaceOrPunctuation
      visit :: EdgeCharacter -> CMark.Node -> EdgeCharacter -> [CMark.Node]
      visit left n@(CMark.Node p typ children) right =
        let emphasised =
              let children' = emphasisOffEdges typ (unwrapAll typ (inside children))
               in if opens left children' && closes children' right
                    then [CMark.Node p typ children']
                    else children'
         in case typ of
              CMark.EMPH -> emphasised
              CMark.STRONG -> emphasised
              CMark.LINK _ _ -> [CMark.Node p typ (inside children)]
              CMark.IMAGE _ _ -> [CMark.Node p typ (inside children)]
              _ -> [n]
   in mergeTexts $ concat (zipWith3 visit lefts nodes rights)

-- | Strong emphasis of emphasis and emphasis of strong emphasis are written the
-- same way, and a reader takes them to be the latter.
strongOutside :: CMark.Node -> CMark.Node
strongOutside = \case
  CMark.Node p CMark.STRONG [CMark.Node q CMark.EMPH cs] ->
    CMark.Node q CMark.EMPH [CMark.Node p CMark.STRONG cs]
  n -> n

-- | The contents of emphasis, with no emphasis at either edge of them.
--
-- The delimiters of emphasis at the edge of emphasis make one run of
-- delimiters, which a reader splits up differently, except for emphasis around
-- nothing but strong emphasis.
emphasisOffEdges :: CMark.NodeType -> [CMark.Node] -> [CMark.Node]
emphasisOffEdges typ contents =
  let unwrapFirst :: [CMark.Node] -> [CMark.Node]
      unwrapFirst = \case
        (CMark.Node _ t cs : rest) | isEmphasis t -> cs ++ rest
        ns -> ns
      unwrapLast :: [CMark.Node] -> [CMark.Node]
      unwrapLast ns = case reverse ns of
        (CMark.Node _ t cs : rest) | isEmphasis t -> reverse rest ++ cs
        _ -> ns
   in case (typ, contents) of
        (CMark.EMPH, [CMark.Node _ CMark.STRONG _]) -> contents
        _ -> mergeTexts (unwrapLast (unwrapFirst contents))

-- | These inlines with no emphasis right next to other emphasis.
--
-- The delimiters of emphasis next to emphasis make one run of delimiters,
-- which a reader splits up differently.
-- Emphasis next to emphasis of the same kind reads the same as one emphasis
-- around both, and of the other kind, it is the second that goes.
separateEmphasis :: [CMark.Node] -> [CMark.Node]
separateEmphasis = \case
  (CMark.Node p a as : CMark.Node _ b bs : rest)
    | isEmphasis a && a == b ->
        separateEmphasis (CMark.Node p a (mergeTexts (as ++ bs)) : rest)
    | isEmphasis a && isEmphasis b ->
        separateEmphasis (mergeTexts (CMark.Node p a as : bs ++ rest))
  (n : rest) -> n : separateEmphasis rest
  [] -> []

-- | Whether a delimiter opens emphasis around these contents.
--
-- Emphasis is never empty, so there is always a first inline to look at.
opens :: EdgeCharacter -> [CMark.Node] -> Bool
opens before contents =
  flip any (take 1 contents) $ \first -> case firstEdge first of
    EdgeAlphanumeric -> True
    EdgeSpaceOrPunctuation -> isSpaceOrPunctuation before
    EdgeUnknown -> isSpaceOrPunctuation before

-- | Whether a delimiter closes emphasis around these contents.
closes :: [CMark.Node] -> EdgeCharacter -> Bool
closes contents after =
  flip any (lastOf contents) $ \final -> case lastEdge final of
    EdgeAlphanumeric -> True
    EdgeSpaceOrPunctuation -> isSpaceOrPunctuation after
    EdgeUnknown -> isSpaceOrPunctuation after

isSpaceOrPunctuation :: EdgeCharacter -> Bool
isSpaceOrPunctuation = \case
  EdgeSpaceOrPunctuation -> True
  EdgeAlphanumeric -> False
  EdgeUnknown -> False

-- | The first character of this inline once it is rendered.
firstEdge :: CMark.Node -> EdgeCharacter
firstEdge (CMark.Node _ typ _) = case typ of
  CMark.TEXT t -> edgeCharacter (Text.take 1 t)
  -- Every other inline this module makes starts with punctuation or a line
  -- ending: a backtick, a delimiter or a bracket.
  _ -> EdgeSpaceOrPunctuation

-- | The last character of this inline once it is rendered.
lastEdge :: CMark.Node -> EdgeCharacter
lastEdge (CMark.Node _ typ _) = case typ of
  CMark.TEXT t -> edgeCharacter (Text.takeEnd 1 t)
  -- Every other inline this module makes ends with punctuation or a line
  -- ending: a backtick, a delimiter or a parenthesis.
  _ -> EdgeSpaceOrPunctuation

-- | What this character, at the edge of some text, is.
--
-- A character that libcmark escapes is ASCII punctuation, or renders as an
-- entity, which starts and ends with punctuation, so escaping does not change
-- which of these a character is.
edgeCharacter :: Text -> EdgeCharacter
edgeCharacter c
  | Text.all isSpace c = EdgeSpaceOrPunctuation
  | Text.all (`elem` asciiPunctuation) c = EdgeSpaceOrPunctuation
  | Text.all isAlphaNum c = EdgeAlphanumeric
  | otherwise = EdgeUnknown

-- | Emphasis around these inlines, with any whitespace at their edges moved
-- outside of it.
--
-- A delimiter that whitespace follows cannot open emphasis, and one that
-- whitespace precedes cannot close it, so emphasis that started or ended with
-- whitespace would render as literal asterisks.
-- Whitespace reads the same just outside the emphasis as just inside it.
delimited :: CMark.NodeType -> [CMark.Node] -> [CMark.Node]
delimited typ contents =
  let (leading, rest) = spanWhitespace contents
      (trailing, middle) = spanWhitespaceEnd rest
   in concat
        [ leading,
          [node typ middle | not (null middle)],
          trailing
        ]

-- | The whitespace at the start of these inlines, and the rest.
spanWhitespace :: [CMark.Node] -> ([CMark.Node], [CMark.Node])
spanWhitespace = \case
  [] -> ([], [])
  (n@(CMark.Node _ typ _) : rest) -> case typ of
    CMark.SOFTBREAK -> let (ws, rest') = spanWhitespace rest in (n : ws, rest')
    CMark.LINEBREAK -> let (ws, rest') = spanWhitespace rest in (n : ws, rest')
    CMark.TEXT t ->
      let (ws, nonWs) = Text.span isSpace t
       in if Text.null nonWs
            then let (ws', rest') = spanWhitespace rest in (textNode ws : ws', rest')
            else ([textNode ws | not (Text.null ws)], textNode nonWs : rest)
    _ -> ([], n : rest)

spanWhitespaceEnd :: [CMark.Node] -> ([CMark.Node], [CMark.Node])
spanWhitespaceEnd ns =
  let (ws, rest) = spanWhitespace (reverse (map reverseText ns))
   in (reverse (map reverseText ws), reverse (map reverseText rest))

-- | These inlines without the spaces and breaks a reader drops from the edges
-- of a paragraph or a heading.
trimEdges :: [CMark.Node] -> [CMark.Node]
trimEdges =
  let trimStart :: [CMark.Node] -> [CMark.Node]
      trimStart = \case
        [] -> []
        (n : rest)
          | isBreak n -> trimStart rest
        (CMark.Node p (CMark.TEXT t) cs : rest) ->
          let t' = Text.dropWhile (== ' ') t
           in if Text.null t'
                then trimStart rest
                else CMark.Node p (CMark.TEXT t') cs : rest
        ns -> ns
      trimEnd :: [CMark.Node] -> [CMark.Node]
      trimEnd = reverse . map reverseText . trimStart . map reverseText . reverse
   in trimEnd . trimStart

-- | A reader drops the spaces at the end of a line and at the start of the
-- next one, and would read two spaces at the end of a line as a hard break.
stripSpacesAroundBreaks :: [CMark.Node] -> [CMark.Node]
stripSpacesAroundBreaks ns =
  let strip :: [CMark.Node] -> CMark.Node -> [CMark.Node] -> CMark.Node
      strip before n after = case n of
        CMark.Node p (CMark.TEXT t) cs ->
          let afterBreak = if any isBreak before then Text.dropWhile (== ' ') t else t
              beforeBreak = if any isBreak after then Text.dropWhileEnd (== ' ') afterBreak else afterBreak
           in CMark.Node p (CMark.TEXT beforeBreak) cs
        _ -> n
   in zipWith3 strip ([] : map pure ns) ns (map pure (drop 1 ns) ++ [[]])

-- | These inlines with adjacent texts joined up, and adjacent code spans too,
-- which a reader cannot tell apart from one code span.
mergeTexts :: [CMark.Node] -> [CMark.Node]
mergeTexts = \case
  (CMark.Node p (CMark.TEXT a) [] : CMark.Node _ (CMark.TEXT b) [] : rest) ->
    mergeTexts (CMark.Node p (CMark.TEXT (Text.append a b)) [] : rest)
  (CMark.Node p (CMark.CODE a) [] : CMark.Node _ (CMark.CODE b) [] : rest) ->
    mergeTexts (CMark.Node p (CMark.CODE (Text.append a b)) [] : rest)
  (n : rest) -> n : mergeTexts rest
  [] -> []

-- | These inlines with each run of breaks made into one break, which is a hard
-- one if any of them was.
--
-- A second break right after a first one would leave a line with nothing on
-- it, which ends the paragraph.
collapseBreaks :: [CMark.Node] -> [CMark.Node]
collapseBreaks = \case
  (a : b : rest)
    | isBreak a && isBreak b ->
        let hard :: CMark.Node -> Bool
            hard (CMark.Node _ typ _) = typ == CMark.LINEBREAK
         in collapseBreaks (node (if hard a || hard b then CMark.LINEBREAK else CMark.SOFTBREAK) [] : rest)
  (n : rest) -> n : collapseBreaks rest
  [] -> []

dropEmptyTexts :: [CMark.Node] -> [CMark.Node]
dropEmptyTexts = filter $ \case
  CMark.Node _ (CMark.TEXT t) _ -> not (Text.null t)
  _ -> True

isEmphasis :: CMark.NodeType -> Bool
isEmphasis = \case
  CMark.EMPH -> True
  CMark.STRONG -> True
  _ -> False

-- | This inline with its text reversed, if it is text.
reverseText :: CMark.Node -> CMark.Node
reverseText = \case
  CMark.Node p (CMark.TEXT t) cs -> CMark.Node p (CMark.TEXT (Text.reverse t)) cs
  n -> n

isBreak :: CMark.Node -> Bool
isBreak (CMark.Node _ typ _) = case typ of
  CMark.SOFTBREAK -> True
  CMark.LINEBREAK -> True
  _ -> False

textNode :: Text -> CMark.Node
textNode t = node (CMark.TEXT t) []

-- | Every character that cannot be written replaced with the replacement
-- character.
--
-- CommonMark has a reader replace every NUL with it, and libcmark fails to
-- write the two noncharacters at the end of the basic multilingual plane.
replaceUnwritable :: Text -> Text
replaceUnwritable = Text.map $ \c ->
  if c `elem` ("\NUL\xFFFE\xFFFF" :: String) then '\xFFFD' else c

-- | Every line ending replaced with a space.
oneLine :: Text -> Text
oneLine = Text.replace "\n" " " . Text.replace "\r" " " . Text.replace "\r\n" " "

-- | Where a link goes, with whitespace percent-encoded as a reader would.
--
-- A link destination cannot hold whitespace, so libcmark percent-encodes it.
urlDestination :: Url -> Text
urlDestination =
  let encode :: Char -> Text
      encode = \case
        '\t' -> "%09"
        '\n' -> "%0A"
        '\v' -> "%0B"
        '\f' -> "%0C"
        '\r' -> "%0D"
        ' ' -> "%20"
        c -> Text.singleton c
   in Text.concatMap encode . replaceUnwritable . unUrl

textLines :: Text -> NonEmpty Text
textLines t =
  let (line, rest) = Text.breakOn "\n" t
   in case Text.stripPrefix "\n" rest of
        Nothing -> line :| []
        Just next -> NE.cons line (textLines next)

node :: CMark.NodeType -> [CMark.Node] -> CMark.Node
node = CMark.Node Nothing

-- | What CommonMark counts as punctuation that a backslash can escape.
asciiPunctuation :: String
asciiPunctuation = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
