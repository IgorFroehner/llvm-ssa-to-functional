
module ParserSpec (spec) where

import Test.Hspec
import Data.Either (isRight)
import Data.List (isInfixOf)
import qualified Data.ByteString.Lazy.Char8 as BL

import Lexer (runAlex)
import Parser (parseLLVMIR)
import Translate (translate)
import PrintAnf (printProgram)

-- | Parse + translate + print, fully forcing the result so that lazily-thrown
-- translation errors (e.g. an unresolved phi predecessor) surface here.
anf :: BL.ByteString -> String
anf src = case printProgram . translate <$> runAlex src parseLLVMIR of
  Left e  -> error ("parse failed: " ++ e)
  Right s -> foldr seq s s

-- A single-block function: LLVM omits the entry label here.
unlabeledEntry :: BL.ByteString
unlabeledEntry = BL.pack "define i32 @square(i32 %0) {\n\
                         \  %2 = mul i32 %0, %0\n\
                         \  ret i32 %2\n\
                         \}\n"

-- An unlabeled entry that branches into labeled blocks.
unlabeledEntryWithBranch :: BL.ByteString
unlabeledEntryWithBranch = BL.pack "define i32 @pick(i32 %0) {\n\
                                   \  %2 = icmp sgt i32 %0, 0\n\
                                   \  br i1 %2, label %pos, label %neg\n\
                                   \pos:\n\
                                   \  ret i32 1\n\
                                   \neg:\n\
                                   \  ret i32 0\n\
                                   \}\n"

-- One numbered arg (%0, slot 0), so the unlabeled entry is %1. The phi in block
-- %2 references the entry by that number: [ %0, %1 ].
phiReferencesEntry :: BL.ByteString
phiReferencesEntry = BL.pack "define i32 @f(i32 %0) {\n\
                             \  br label %2\n\
                             \2:\n\
                             \  %3 = phi i32 [ %0, %1 ], [ %3, %2 ]\n\
                             \  ret i32 %3\n\
                             \}\n"

-- A *named* arg (%n) consumes no implicit slot, so the unlabeled entry is %0.
-- The phi references the entry as %0.
namedArg :: BL.ByteString
namedArg = BL.pack "define i32 @g(i32 %n) {\n\
                   \  br label %1\n\
                   \1:\n\
                   \  %2 = phi i32 [ %n, %0 ], [ %2, %1 ]\n\
                   \  ret i32 %2\n\
                   \}\n"

spec :: Spec
spec = parallel $ do
  describe "unlabeled entry block" $ do
    it "parses a function whose entry block has no label" $ do
      runAlex unlabeledEntry parseLLVMIR `shouldSatisfy` isRight

    it "handles an unlabeled entry that branches to labeled blocks" $ do
      runAlex unlabeledEntryWithBranch parseLLVMIR `shouldSatisfy` isRight

    it "labels the entry with its implicit LLVM number so phis resolve" $ do
      -- entry is %1 (one numbered arg before it); the phi's [ %0, %1 ] resolves
      -- to %0 when the entry calls block %2.
      let out = anf phiReferencesEntry
      out `shouldSatisfy` isInfixOf "a1 ()"   -- entry lambda named a1
      out `shouldSatisfy` isInfixOf "a2 a0"   -- entry tail-calls a2 with %0

    it "ignores named arguments when numbering the entry block" $ do
      -- %n is named, so it consumes no slot and the entry is %0 (not %1).
      let out = anf namedArg
      out `shouldSatisfy` isInfixOf "a0 ()"   -- entry lambda named a0
      out `shouldSatisfy` isInfixOf "a1 an"   -- entry tail-calls a1 with %n
