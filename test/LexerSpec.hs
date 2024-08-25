
module LexerSpec (spec) where

import Test.Hspec
import Data.Either (isRight, isLeft)

import Lexer

spec :: Spec
spec = parallel $ do
  describe "scanMany" $ do
    it "returns Right for valid tokens" $ do
      scanMany "loop_start: %1 define" `shouldSatisfy` isRight

    it "returns Left for invalid tokens" $ do
      scanMany "asdf" `shouldSatisfy` isLeft
