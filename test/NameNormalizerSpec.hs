-- | Unit spec for NameNormalizer: LLVM identifiers must normalize to valid
-- Haskell identifiers, stripping every punctuation character the lexer admits
-- inside a name (sigils, '.', '$', the label colon). A def and its references
-- must collapse to the same string, which is what makes the generated Haskell
-- well-scoped.
module NameNormalizerSpec (spec) where

import Test.Hspec
import Data.Char (isAlphaNum)

import NameNormalizer

spec :: Spec
spec = parallel $ do
  describe "NameNormalizer" $ do

    describe "normalizeName (local registers / block labels)" $ do
      it "prefixes 'a' and strips the leading % sigil" $
        normalizeName "%3" `shouldBe` "a3"
      it "strips '.' so dotted labels become valid identifiers" $
        normalizeName "%for.cond.cleanup" `shouldBe` "aforcondcleanup"
      it "strips '$' (an operator char in Haskell)" $
        normalizeName "%for$body" `shouldBe` "aforbody"
      it "collapses a label def (trailing colon) to its reference form" $
        normalizeName "for.cond.cleanup:" `shouldBe` normalizeName "%for.cond.cleanup"

    describe "normalizeGlobal (function names)" $ do
      it "strips '$' from globals too" $
        normalizeGlobal "@foo$bar" `shouldBe` "foobar"

    describe "output is always a valid Haskell identifier body" $ do
      it "leaves only alphanumerics and underscores" $ do
        let ok s = all (\c -> isAlphaNum c || c == '_') s
        normalizeName "%a.b$c:" `shouldSatisfy` ok
        normalizeGlobal "@x$y.z" `shouldSatisfy` ok
