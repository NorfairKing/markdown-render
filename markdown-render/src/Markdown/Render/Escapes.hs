{-# LANGUAGE OverloadedStrings #-}

-- | Leaving out the backslash escapes that a reader does not need.
module Markdown.Render.Escapes (fewerEscapes) where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as Text
import Markdown.Render.Node (asciiPunctuation)

-- | This text with every backslash escape left out that the given test still
-- accepts it without.
--
-- libcmark escapes every character that is markup somewhere, rather than
-- only where it would be markup, which leaves a reader of the text itself
-- wading through backslashes.
--
-- Leaving out one escape can make another one necessary, so each one that
-- goes is checked together with all the ones that went before it.
-- All of them are tried at once first, and then each half of them, so that
-- text that needs none of its escapes is only tested once.
fewerEscapes :: (Text -> Bool) -> Text -> Text
fewerEscapes accepts t =
  let segments = splitAtEscapes t
      render :: IntSet -> Text
      render dropped = withEscapes dropped segments
      search :: IntSet -> NonEmpty Int -> IntSet
      search dropped candidates =
        let allDropped = IntSet.union dropped (IntSet.fromList (NE.toList candidates))
         in if accepts (render allDropped)
              then allDropped
              else case candidates of
                _ :| [] -> dropped
                first :| (second : rest) ->
                  let (these, those) = halves rest
                   in search (search dropped (first :| these)) (second :| those)
      _ :| escaped = segments
   in case NE.nonEmpty (zipWith const [0 ..] escaped) of
        Nothing -> t
        Just candidates -> render (search IntSet.empty candidates)

-- | The text split at every backslash that escapes a character, without those
-- backslashes, so that each segment after the first starts with the character
-- that was escaped.
--
-- A backslash in front of anything other than ASCII punctuation, like the
-- line ending of a hard break, escapes nothing and stays in its segment.
splitAtEscapes :: Text -> NonEmpty Text
splitAtEscapes t =
  let (before, fromBackslash) = Text.breakOn "\\" t
   in case Text.uncons (Text.drop 1 fromBackslash) of
        Nothing -> t :| []
        Just (c, after) ->
          let next :| more = splitAtEscapes after
           in if c `elem` asciiPunctuation
                then before :| (Text.cons c next : more)
                else Text.concat [before, "\\", Text.singleton c, next] :| more

-- | The segments joined up again, with a backslash in front of every one but
-- the first, unless it is one of the ones to leave out.
withEscapes :: IntSet -> NonEmpty Text -> Text
withEscapes dropped (first :| escaped) =
  Text.concat $
    first
      : concat
        ( zipWith
            (\i segment -> [if IntSet.member i dropped then "" else "\\", segment])
            [0 ..]
            escaped
        )

-- | Every other element, and the rest.
halves :: [a] -> ([a], [a])
halves = foldr (\x (these, those) -> (x : those, these)) ([], [])
