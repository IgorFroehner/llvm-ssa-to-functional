
module LexerSpec (spec) where

import Test.Hspec
import Data.Either (isRight, isLeft)
import Data.ByteString.Lazy.Char8 (ByteString)

import Lexer

verifyEither :: Either String [a] -> [a]
verifyEither = either (error "either was left") id

extractTokens :: [RangedToken] -> [Token]
extractTokens = map rtToken

scanTokens :: ByteString -> [Token]
scanTokens str = extractTokens $ verifyEither (scanMany str)

spec :: Spec
spec = parallel $ do
  describe "scanMany" $ do
    it "returns Left for invalid tokens" $ do
      scanMany "asdf" `shouldSatisfy` isLeft

    it "returns Right for valid tokens" $ do
      scanMany "loop_start: %1 define" `shouldSatisfy` isRight

    it "returns expected valid assing token" $ do
      scanTokens "=" `shouldBe` [Assign, EOF]
    
    it "returns expected valid curly brackets" $ do
      scanTokens "{ }" `shouldBe` [LCurlyBracket, RCurlyBracket, EOF]

    it "returns expected valid parentheses" $ do
      scanTokens "( )" `shouldBe` [LPar, RPar, EOF]

    it "returns expected valid brackets" $ do
      scanTokens "[ ]" `shouldBe` [LBrack, RBrack, EOF]

    it "returns expected valid comma" $ do
      scanTokens "," `shouldBe` [Comma, EOF]
    
    it "returns expected BasicBlock" $ do
      scanTokens "asdf:" `shouldBe` [BasicBlock "asdf:", EOF]

    it "returns expected local variable" $ do
      scanTokens "%asdf" `shouldBe` [LIdentifier "%asdf", EOF]

    it "returns expected global variable" $ do
      scanTokens "@asdf" `shouldBe` [GIdentifier "@asdf", EOF]
