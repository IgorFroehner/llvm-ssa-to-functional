
module ParserSpec (spec) where

import Test.Hspec
import Data.Either (isRight)
import Data.List (isInfixOf)
import qualified Data.ByteString.Lazy.Char8 as BL

import Lexer (runAlex)
import Parser (parseLLVMIR)
import Translate (translate)
import PrintAnf (printProgram)

-- | Parse + translate + print, starting from LLVM-IR source text.
compileToAnf :: BL.ByteString -> Either String String
compileToAnf src = printProgram . translate <$> runAlex src parseLLVMIR

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

spec :: Spec
spec = parallel $ do
  describe "unlabeled entry block" $ do
    it "parses a function whose entry block has no label" $ do
      runAlex unlabeledEntry parseLLVMIR `shouldSatisfy` isRight

    it "synthesizes an 'entryblock' lambda for the unlabeled entry" $ do
      compileToAnf unlabeledEntry `shouldSatisfy` either (const False) ("entryblock" `isInfixOf`)

    it "handles an unlabeled entry that branches to labeled blocks" $ do
      runAlex unlabeledEntryWithBranch parseLLVMIR `shouldSatisfy` isRight
