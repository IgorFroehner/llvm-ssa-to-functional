
module LexerSpec (spec) where

import Test.Hspec
import Data.Either (isRight, isLeft)

import Lexer

spec :: Spec
spec = parallel $ do
  describe "scanMany" $ do
    it "returns Right for valid tokens" $ do
      scanMany "loop_start: %1 define" `shouldSatisfy` isRight
    
    -- it "returns expected valid Assign token" $ do
    --   scanMany "=" `shouldBe` Right [
    --     RangedToken {rtToken = Assign, 
    --                  rtRange = Range
    --                  {start = AlexPn 0 1 1,
    --                   stop = AlexPn 1 1 2}},
    --     RangedToken {rtToken = EOF,
    --                  rtRange = Range
    --                  {start = AlexPn 1 1 2,
    --                  stop = AlexPn 1 1 2}}]

    it "returns Left for invalid tokens" $ do
      scanMany "asdf" `shouldSatisfy` isLeft
