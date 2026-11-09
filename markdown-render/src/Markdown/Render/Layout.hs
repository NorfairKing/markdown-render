{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | How the blocks of a document are laid out in lines.
--
-- [check:file] Mutation testing is off for this module, because the golden
-- tests of the rendering pin every line of it down.
-- Add a golden test for any new case.
module Markdown.Render.Layout (blockChunks) where

import qualified CMark
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Text
import Markdown.Render.Escapes (fewerEscapes)
import Markdown.Render.Node (asciiPunctuation, node)

{-# ANN module ("DisableMutations" :: String) #-}

-- | The rendering of a document's top-level blocks, one per block.
blockChunks :: [CMark.Node] -> [Text]
blockChunks blocks =
  zipWith
    (\separator ls -> Text.concat [separator, Text.unlines ls])
    ("" : repeat "\n")
    (blockLineGroups blocks)

-- | How the blocks in a sequence are separated: by a blank line, or by nothing.
data Separation = SeparatedByBlankLines | SeparatedByNothing

-- | The lines of a sequence of blocks.
sequenceLines :: Separation -> [CMark.Node] -> [Text]
sequenceLines separation blocks =
  let groups = blockLineGroups blocks
   in case separation of
        SeparatedByBlankLines -> intercalate [""] groups
        SeparatedByNothing -> concat groups

-- | The lines of each block in a sequence.
--
-- Two bullet lists in a row would read as one, unless the second one uses a
-- different bullet.
-- Ordered lists in a row are kept apart in the tree already, by their
-- delimiters.
blockLineGroups :: [CMark.Node] -> [[Text]]
blockLineGroups =
  let isBulletList :: CMark.Node -> Bool
      isBulletList (CMark.Node _ typ _) = case typ of
        CMark.LIST attributes -> CMark.listType attributes == CMark.BULLET_LIST
        _ -> False
      go :: Bullet -> [CMark.Node] -> [[Text]]
      go bullet = \case
        [] -> []
        (b : rest) ->
          let nextBullet =
                if isBulletList b
                  then otherBullet bullet
                  else BulletDash
           in blockLines bullet b : go nextBullet rest
   in go BulletDash

data Bullet = BulletDash | BulletPlus

otherBullet :: Bullet -> Bullet
otherBullet = \case
  BulletDash -> BulletPlus
  BulletPlus -> BulletDash

bulletMarker :: Bullet -> Text
bulletMarker = \case
  BulletDash -> "- "
  BulletPlus -> "+ "

-- | The lines of one block, which a bullet list marks its items with the given
-- bullet in.
--
-- The text in a block is libcmark's to render, because that is where the
-- escaping is.
-- How blocks sit in other blocks is not, because libcmark lays out lists in
-- ways that a reader does not read back as the same lists.
blockLines :: Bullet -> CMark.Node -> [Text]
blockLines bullet n@(CMark.Node _ typ children) = case typ of
  CMark.LIST attributes ->
    let markers = case CMark.listType attributes of
          CMark.BULLET_LIST -> repeat (bulletMarker bullet)
          CMark.ORDERED_LIST ->
            let delimiter = case CMark.listDelim attributes of
                  CMark.PERIOD_DELIM -> ". "
                  CMark.PAREN_DELIM -> ") "
             in map (\i -> Text.concat [Text.pack (show i), delimiter]) [1 :: Word ..]
        separation :: Separation
        separation =
          if CMark.listTight attributes
            then SeparatedByNothing
            else SeparatedByBlankLines
        itemLines :: Text -> CMark.Node -> [Text]
        itemLines marker (CMark.Node _ _ blocks) = markedLines marker (sequenceLines separation blocks)
        items = zipWith itemLines markers children
     in case separation of
          SeparatedByBlankLines -> intercalate [""] items
          SeparatedByNothing -> concat items
  CMark.BLOCK_QUOTE ->
    case sequenceLines SeparatedByBlankLines children of
      [] -> [">"]
      ls -> map (\l -> if Text.null l then ">" else Text.append "> " l) ls
  -- libcmark writes a thematic break as a line of dashes, which right under a
  -- line of text would underline it as a heading instead.
  CMark.THEMATIC_BREAK -> ["***"]
  CMark.CODE_BLOCK info contents -> codeBlockLines info contents
  -- libcmark leaves tildes at the start of a line as they are, but a run of
  -- them would open a code block.
  CMark.PARAGRAPH ->
    necessaryEscapes n $
      map
        (\l -> if "~" `Text.isPrefixOf` l then Text.cons '\\' l else l)
        (cmarkLines n)
  _ -> necessaryEscapes n (cmarkLines n)

-- | The lines of a list item's contents, behind its marker.
markedLines :: Text -> [Text] -> [Text]
markedLines marker =
  let indentation = Text.replicate (Text.length marker) " "
   in zipWith
        ( \isFirst l ->
            if isFirst
              then Text.append marker l
              else if Text.null l then l else Text.append indentation l
        )
        (True : repeat False)

-- | These lines of a block without the escapes that a reader of the block does
-- not need to read it as this block.
--
-- Inside a quote, a block reads the same as it does on its own.
-- Inside a list item it does not, because its first line can make something
-- else together with the marker in front of it, like a thematic break out of
-- @- --@, so it is read behind each marker it could be behind as well.
necessaryEscapes :: CMark.Node -> [Text] -> [Text]
necessaryEscapes n =
  let expected = withoutPositions n
      readBack :: Text -> CMark.Node
      readBack = withoutPositions . CMark.commonmarkToNode [CMark.optNormalize]
      readsAlone :: [Text] -> Bool
      readsAlone ls = case readBack (Text.unlines ls) of
        CMark.Node _ CMark.DOCUMENT [block] -> block == expected
        _ -> False
      readsAsItem :: [Text] -> Text -> Bool
      readsAsItem ls marker = case readBack (Text.unlines (markedLines marker ls)) of
        CMark.Node _ CMark.DOCUMENT [CMark.Node _ (CMark.LIST _) [CMark.Node _ CMark.ITEM [block]]] -> block == expected
        _ -> False
      readsBack :: Text -> Bool
      readsBack t =
        let ls = Text.lines t
         in readsAlone ls && all (readsAsItem ls) [bulletMarker BulletDash, bulletMarker BulletPlus, "1. ", "1) "]
   in Text.lines . fewerEscapes readsBack . Text.unlines

-- | The tree without where its nodes came from.
withoutPositions :: CMark.Node -> CMark.Node
withoutPositions (CMark.Node _ typ children) =
  CMark.Node Nothing typ (map withoutPositions children)

cmarkLines :: CMark.Node -> [Text]
cmarkLines n = Text.lines (CMark.nodeToCommonmark [] noWrapping (node CMark.DOCUMENT [n]))

-- | A fenced code block.
--
-- libcmark writes the info string as it is, but a reader takes a backslash or
-- an ampersand in it to start an escape, and a backtick in it to mean it is not
-- a fence at all.
codeBlockLines :: Text -> Text -> [Text]
codeBlockLines info contents =
  let fenceCharacter = if Text.any (== '`') info then '~' else '`'
      longestRun :: Int
      longestRun =
        foldl' max 0 $
          map Text.length $
            filter (Text.all (== fenceCharacter)) (Text.group contents)
      -- Longer than any run of fence characters in the contents, so that no
      -- line of them closes the block early.
      fence = Text.replicate (max 3 (longestRun + 1)) (Text.singleton fenceCharacter)
      escape :: Char -> Text
      escape c =
        if c `elem` asciiPunctuation
          then Text.pack ['\\', c]
          else Text.singleton c
   in concat
        [ [Text.append fence (Text.concatMap escape info)],
          Text.lines contents,
          [fence]
        ]

-- | libcmark's way of saying not to wrap lines.
noWrapping :: Maybe Int
noWrapping = Nothing
