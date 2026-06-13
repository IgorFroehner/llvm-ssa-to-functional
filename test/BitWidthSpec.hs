-- | TDD spec for docs/roadmap/bit-width-fidelity.md.
--
-- These tests pin down the *target* behaviour of bit-width-faithful codegen:
-- each LLVM @iN@ must map to the corresponding sized Haskell integer
-- (@Int8@\/@Int16@\/@Int32@\/@Int64@) instead of collapsing into @Int@, so
-- wraparound, @trunc@, @zext@ and @sext@ are semantically correct.
--
-- They drive the real pipeline (lex -> parse -> translate -> print) and assert
-- on the emitted Haskell. They encode the acceptance criteria for the feature
-- (see the roadmap item) and are the unit-level complement to the end-to-end
-- differential harness in test/differential/.
module BitWidthSpec (spec) where

import Test.Hspec
import Data.List (isInfixOf)
import qualified Data.ByteString.Lazy.Char8 as BL

import Lexer (runAlex)
import Parser (parseLLVMIR)
import Translate (translate)
import PrintAnf (printProgram)

-- | Run the whole pipeline on an LLVM-IR source string, returning the emitted
-- Haskell. Errors loudly if the snippet fails to parse.
emit :: String -> String
emit src = case runAlex (BL.pack src) parseLLVMIR of
  Left err  -> error ("parse failed: " ++ err)
  Right ast -> printProgram (translate ast)

-- A tiny i32 identity-ish kernel with one arithmetic binding.
add1 :: String
add1 = unlines
  [ "define i32 @add1(i32 %0) {"
  , "  %2 = add i32 %0, 1"
  , "  ret i32 %2"
  , "}"
  ]

-- i32 in, i64 out, via sext: exercises a width-widening conv op.
sextSquare :: String
sextSquare = unlines
  [ "define i64 @no_overflow_square(i32 %x) {"
  , "  %1 = sext i32 %x to i64"
  , "  %2 = mul i64 %1, %1"
  , "  ret i64 %2"
  , "}"
  ]

-- zext i32 -> i33 (non-power-of-two width), then trunc i33 -> i32. The i33
-- intermediate must round up to Int64 so the product does not overflow before
-- being truncated back to i32. Lifted from examples/sum.ll.
zextTrunc :: String
zextTrunc = unlines
  [ "define i32 @widen(i32 %0) {"
  , "  %1 = zext i32 %0 to i33"
  , "  %2 = mul i33 %1, %1"
  , "  %3 = trunc i33 %2 to i32"
  , "  ret i32 %3"
  , "}"
  ]

spec :: Spec
spec = parallel $ do
  describe "bit-width fidelity" $ do

    describe "type mapping" $ do
      it "emits a top-level signature mapping i32 -> Int32" $
        emit add1 `shouldSatisfy` isInfixOf "add1 :: Int32 -> Int32"

      it "annotates arithmetic bindings with their iN width" $
        emit add1 `shouldSatisfy` isInfixOf ":: Int32"

      it "imports the sized integer types" $
        emit add1 `shouldSatisfy` isInfixOf "import Data.Int"

      it "maps i64 returns to Int64 in the signature" $
        emit sextSquare `shouldSatisfy` isInfixOf "no_overflow_square :: Int32 -> Int64"

    describe "conversion ops actually convert" $ do
      it "sext widens with a fromIntegral into the target width" $ do
        let out = emit sextSquare
        out `shouldSatisfy` isInfixOf "fromIntegral"
        out `shouldSatisfy` isInfixOf ":: Int64"

      it "rounds the non-power-of-two i33 up to Int64" $
        emit zextTrunc `shouldSatisfy` isInfixOf "Int64"

      it "zext goes through the unsigned word type" $
        emit zextTrunc `shouldSatisfy` isInfixOf "Word32"

      it "trunc narrows back to the i32 target" $
        emit zextTrunc `shouldSatisfy` isInfixOf ":: Int32"
