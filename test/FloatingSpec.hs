-- | TDD spec for docs/roadmap/plans/09-floating-point.md.
--
-- Pins down the floating-point fragment: @float@\/@double@ map to Haskell
-- @Float@\/@Double@, the @f*@ instruction family translates with the right
-- operators/comparisons, the int<->float conversions emit the correct (and
-- correctly-rounding) coercions, and floating literals are emitted as
-- explicitly-typed Haskell literals. Drives the real pipeline (lex -> parse ->
-- translate -> print) and asserts on the emitted Haskell; the end-to-end
-- bit-exact check lives in test/differential/.
module FloatingSpec (spec) where

import Test.Hspec
import Data.List (isInfixOf)
import qualified Data.ByteString.Lazy.Char8 as BL

import Lexer (runAlex)
import Parser (parseLLVMIR)
import Translate (translate)
import PrintAnf (printProgram)

emit :: String -> String
emit src = case runAlex (BL.pack src) parseLLVMIR of
  Left err  -> error ("parse failed: " ++ err)
  Right ast -> printProgram (translate ast)

-- double arithmetic + a literal operand.
fadd :: String
fadd = unlines
  [ "define double @f(double %a, double %b) {"
  , "  %1 = fadd double %a, %b"
  , "  %2 = fmul double %1, 2.000000e+00"
  , "  ret double %2"
  , "}"
  ]

fdiv :: String
fdiv = unlines
  [ "define double @g(double %a, double %b) {"
  , "  %1 = fdiv double %a, %b"
  , "  ret double %1"
  , "}"
  ]

-- fcmp with an unordered predicate (clang's lowering of a `<=` guard).
fcmpUgt :: String
fcmpUgt = unlines
  [ "define double @h(double %a) {"
  , "  %1 = fcmp ugt double %a, 0.000000e+00"
  , "  ret double %a"
  , "}"
  ]

-- single-precision: float arithmetic must map to Float, not Double.
floatArith :: String
floatArith = unlines
  [ "define float @s(float %a, float %b) {"
  , "  %1 = fadd float %a, %b"
  , "  ret float %1"
  , "}"
  ]

-- the int<->float conversions, including the rounding-sensitive fptosi.
sitofpCase :: String
sitofpCase = unlines
  [ "define double @i2f(i32 %n) {"
  , "  %1 = sitofp i32 %n to double"
  , "  ret double %1"
  , "}"
  ]

uitofpCase :: String
uitofpCase = unlines
  [ "define double @u2f(i32 %n) {"
  , "  %1 = uitofp i32 %n to double"
  , "  ret double %1"
  , "}"
  ]

fptosiCase :: String
fptosiCase = unlines
  [ "define i32 @f2i(double %x) {"
  , "  %1 = fptosi double %x to i32"
  , "  ret i32 %1"
  , "}"
  ]

fpextCase :: String
fpextCase = unlines
  [ "define double @ext(float %x) {"
  , "  %1 = fpext float %x to double"
  , "  ret double %1"
  , "}"
  ]

fptruncCase :: String
fptruncCase = unlines
  [ "define float @tr(double %x) {"
  , "  %1 = fptrunc double %x to float"
  , "  ret float %1"
  , "}"
  ]

spec :: Spec
spec = parallel $ do
  describe "floating point" $ do

    describe "types and signatures" $ do
      it "maps double to Double in the signature" $
        emit fadd `shouldSatisfy` isInfixOf "f :: Double -> Double -> Double"
      it "maps float to Float in the signature" $
        emit floatArith `shouldSatisfy` isInfixOf "s :: Float -> Float -> Float"
      it "annotates double bindings with :: Double" $
        emit fadd `shouldSatisfy` isInfixOf ":: Double"

    describe "arithmetic and comparison" $ do
      it "maps fadd/fmul to + and *" $ do
        let out = emit fadd
        out `shouldSatisfy` isInfixOf " + "
        out `shouldSatisfy` isInfixOf " * "
      it "maps fdiv to / (true division, not quot)" $ do
        let out = emit fdiv
        out `shouldSatisfy` isInfixOf " / "
        out `shouldNotSatisfy` isInfixOf "quot"
      it "maps the unordered fcmp ugt to >" $
        emit fcmpUgt `shouldSatisfy` isInfixOf " > "

    describe "literals" $ do
      it "emits floating literals as explicitly-typed Haskell literals" $
        emit fadd `shouldSatisfy` isInfixOf "(2.000000e+00 :: Double)"

    describe "conversions" $ do
      it "sitofp is a plain fromIntegral into the float type" $
        emit sitofpCase `shouldSatisfy` isInfixOf "fromIntegral"
      it "uitofp zero-extends through the unsigned word type" $
        emit uitofpCase `shouldSatisfy` isInfixOf "Word32"
      it "fptosi truncates toward zero (truncate, not round)" $ do
        let out = emit fptosiCase
        out `shouldSatisfy` isInfixOf "truncate"
        out `shouldSatisfy` isInfixOf ":: Int32"
      it "fpext widens with float2Double" $
        emit fpextCase `shouldSatisfy` isInfixOf "float2Double"
      it "fptrunc narrows with double2Float" $
        emit fptruncCase `shouldSatisfy` isInfixOf "double2Float"
