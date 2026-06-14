-- | TDD spec for docs/roadmap/10-boolean-types.md: the i1-aware translation
-- that keeps an @i1@ as a Haskell 'Bool' rather than re-encoding it through
-- 'Int'. Drives the real pipeline and asserts on the emitted Haskell.
module BooleanSpec (spec) where

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

-- A comparison result widened to an integer: clang's `zext i1 ... to i32`.
zextExample :: String
zextExample = unlines
  [ "define i32 @to_int(i32 %x) {"
  , "  %2 = icmp sgt i32 %x, 0"
  , "  %3 = zext i1 %2 to i32"
  , "  ret i32 %3"
  , "}"
  ]

-- A conditional branch on a comparison result.
branchExample :: String
branchExample = unlines
  [ "define i32 @pick(i32 %x) {"
  , "  %2 = icmp sgt i32 %x, 0"
  , "  br i1 %2, label %t, label %f"
  , "t:"
  , "  ret i32 1"
  , "f:"
  , "  ret i32 0"
  , "}"
  ]

-- `select i1` on a comparison result.
selectExample :: String
selectExample = unlines
  [ "define i32 @clamp(i32 %x) {"
  , "  %2 = icmp slt i32 %x, 0"
  , "  %3 = select i1 %2, i32 0, i32 %x"
  , "  ret i32 %3"
  , "}"
  ]

-- Boolean connectives on i1 (clang's `or i1` / `and i1` / `xor i1`).
boolOpsExample :: String
boolOpsExample = unlines
  [ "define i1 @between(i32 %x) {"
  , "  %2 = icmp sgt i32 %x, 0"
  , "  %3 = icmp slt i32 %x, 10"
  , "  %4 = and i1 %2, %3"
  , "  %5 = xor i1 %4, true"
  , "  ret i1 %5"
  , "}"
  ]

spec :: Spec
spec = describe "i1-aware Bool" $ do
  it "renders a comparison result as a bare Bool, not 0/1" $ do
    let out = emit zextExample
    out `shouldSatisfy` isInfixOf "(ax > 0) :: Bool"
    out `shouldSatisfy` (not . isInfixOf "if ax > 0 then 1 else 0")

  it "reintroduces the integer at a zext i1 boundary" $
    emit zextExample `shouldSatisfy` isInfixOf "(if a2 then 1 else 0) :: Int32"

  it "feeds a Bool condition straight into a branch (no /= 0)" $ do
    let out = emit branchExample
    out `shouldSatisfy` isInfixOf "if a2"
    out `shouldSatisfy` (not . isInfixOf "/= 0")

  it "feeds a Bool condition straight into a select (no /= 0)" $ do
    let out = emit selectExample
    out `shouldSatisfy` isInfixOf "if a2 then 0 else ax"
    out `shouldSatisfy` (not . isInfixOf "/= 0")

  it "lowers i1 and/or/xor to the logical connectives" $ do
    let out = emit boolOpsExample
    out `shouldSatisfy` isInfixOf "(a2 && a3) :: Bool"
    out `shouldSatisfy` isInfixOf "(a4 /= True) :: Bool"

  it "gives an i1 return type a Bool signature" $
    emit boolOpsExample `shouldSatisfy` isInfixOf "between :: Int32 -> Bool"

  it "renders an i1 false literal as a Bool, not 0" $ do
    -- A phi over i1 carries a `false`; under #10 it is `False`, not `0`.
    let out = emit (unlines
          [ "define i1 @f(i32 %x) {"
          , "  %2 = icmp sgt i32 %x, 0"
          , "  br i1 %2, label %t, label %e"
          , "t:"
          , "  br label %e"
          , "e:"
          , "  %3 = phi i1 [ true, %t ], [ false, %0 ]"
          , "  ret i1 %3"
          , "}"
          ])
    out `shouldSatisfy` isInfixOf "False"
    out `shouldSatisfy` isInfixOf "True"
