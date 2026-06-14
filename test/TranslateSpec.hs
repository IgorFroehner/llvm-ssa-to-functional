
module TranslateSpec (spec) where

import Test.Hspec
import Data.List (isInfixOf)
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Ast
import qualified Anf
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

-- Arithmetic shift right: the `ashr` binop variant (docs/roadmap/03).
ashr :: String
ashr = unlines
  [ "define i32 @arith_shr(i32 %x, i32 %n) {"
  , "  %3 = ashr i32 %x, %n"
  , "  ret i32 %3"
  , "}"
  ]

-- `freeze` is the identity on its operand in the pure subset.
freeze :: String
freeze = unlines
  [ "define i32 @freeze_inc(i32 %x) {"
  , "  %2 = freeze i32 %x"
  , "  %3 = add i32 %2, 1"
  , "  ret i32 %3"
  , "}"
  ]

-- The llvm.smin / llvm.smax integer intrinsics clang emits at -O1.
minmax :: String
minmax = unlines
  [ "define i32 @imin(i32 %a, i32 %b) {"
  , "  %3 = call i32 @llvm.smin.i32(i32 %a, i32 %b)"
  , "  ret i32 %3"
  , "}"
  , "define i32 @imax(i32 %a, i32 %b) {"
  , "  %3 = call i32 @llvm.smax.i32(i32 %a, i32 %b)"
  , "  ret i32 %3"
  , "}"
  ]

-- llvm.abs carries a trailing i1 poison immarg that has no Haskell counterpart.
iabs :: String
iabs = unlines
  [ "define i32 @iabs(i32 %x) {"
  , "  %2 = call i32 @llvm.abs.i32(i32 %x, i1 true)"
  , "  ret i32 %2"
  , "}"
  ]

-- A same-module call that is *not* an intrinsic must pass through unchanged.
plainCall :: String
plainCall = unlines
  [ "define i32 @caller(i32 %x) {"
  , "  %2 = call i32 @helper(i32 %x)"
  , "  ret i32 %2"
  , "}"
  ]

spec :: Spec
spec = parallel $ do
  describe "translate" $ do
    it "translates correctly a empty program" $ do
      let ast = Ast.Program []
      translate ast `shouldBe` Anf.Program []

  -- docs/roadmap/03-broader-subset.md: cheap wins that widen the accepted
  -- subset without a second value type. Drive the real pipeline and assert on
  -- the emitted Haskell.
  describe "broader subset" $ do
    -- The shift amount is coerced to Int because Haskell's shiftR takes an Int,
    -- while LLVM types both shift operands at the same iN.
    it "maps ashr to a (sign-propagating) shiftR" $
      emit ashr `shouldSatisfy` isInfixOf "ax `shiftR` (fromIntegral an)"

    it "lowers freeze to a typed identity alias" $ do
      let out = emit freeze
      out `shouldSatisfy` isInfixOf "a2 = (ax) :: Int32"
      out `shouldSatisfy` isInfixOf "a3 = (a2 + 1)"

    it "rewrites llvm.smin / llvm.smax to min / max" $ do
      let out = emit minmax
      out `shouldSatisfy` isInfixOf "min aa ab"
      out `shouldSatisfy` isInfixOf "max aa ab"

    it "rewrites llvm.abs to abs, dropping the i1 immarg" $ do
      let out = emit iabs
      out `shouldSatisfy` isInfixOf "abs ax"
      out `shouldSatisfy` (not . isInfixOf "llvmabs")

    it "leaves non-intrinsic same-module calls untouched" $
      emit plainCall `shouldSatisfy` isInfixOf "helper ax"

    it "translates an i1-returning icmp to a Haskell Bool" $ do
      let out = emit "define i1 @is_positive(i32 %x) {\n  %2 = icmp sgt i32 %x, 0\n  ret i1 %2\n}\n"
      out `shouldSatisfy` isInfixOf "is_positive :: Int32 -> Bool"
      out `shouldSatisfy` isInfixOf "(ax > 0) :: Bool"
      out `shouldSatisfy` (not . isInfixOf "then 1 else 0")
