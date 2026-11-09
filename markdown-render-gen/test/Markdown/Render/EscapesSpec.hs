{-# LANGUAGE OverloadedStrings #-}

module Markdown.Render.EscapesSpec (spec) where

import qualified Data.Text as Text
import Markdown.Render.Escapes
import Markdown.Render.Gen (genMarkdownText)
import Test.QuickCheck (forAll)
import Test.Syd

spec :: Spec
spec =
  describe "fewerEscapes" $ do
    it "keeps every escape when nothing with fewer is accepted" $
      forAll genMarkdownText $ \t ->
        fewerEscapes (const False) t `shouldBe` t

    it "leaves out every escape when anything is accepted" $
      fewerEscapes (const True) "a\\*b\\\\c\\\nd\\e\\"
        `shouldBe` "a*b\\c\\\nd\\e\\"

    it "keeps the one escape that is needed" $
      fewerEscapes ("\\[" `Text.isInfixOf`) "\\[a\\]\\_"
        `shouldBe` "\\[a]_"

    it "keeps the escapes that are needed wherever they are" $
      fewerEscapes (\t -> "\\(" `Text.isInfixOf` t && "\\]" `Text.isInfixOf` t) "\\(\\[\\_\\]\\)\\*"
        `shouldBe` "\\([_\\])*"

    it "keeps an escape that becomes needed once another one is left out" $
      -- Either escape will do, but not neither.
      fewerEscapes (\t -> Text.count "\\" t >= 1) "\\*\\*"
        `shouldBe` "*\\*"

    it "returns what the test accepts, when it accepts what it was given" $
      forAll genMarkdownText $ \t ->
        let accepts u = Text.length u >= Text.length t - 1
         in fewerEscapes accepts t `shouldSatisfy` accepts
