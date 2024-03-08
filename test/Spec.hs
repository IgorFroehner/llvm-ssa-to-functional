import Test.Hspec
import Data.Either (isRight, isLeft)
import qualified Data.ByteString.Lazy as BL

import Lexer
import Parser

main :: IO ()
main = hspec $ do
  describe "Lexer.scanMany" $ do
    it "returns Right for valid tokens" $ do
      scanMany "loop_start: %1 define" `shouldSatisfy` isRight

    it "returns Left for invalid tokens" $ do
      scanMany "asdf" `shouldSatisfy` isLeft

  describe "Parser.parseLLVMIR" $ do
    it "parses a valid LLVM IR" $ do
      s <- BL.readFile "examples/pow_loops.ll"
      runAlex s parseLLVMIR `shouldSatisfy` isRight
