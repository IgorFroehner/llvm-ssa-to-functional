import Test.Hspec
import Data.Either (isRight, isLeft)
import qualified Data.ByteString.Lazy as BL
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Control.Monad (forM_)

import Lexer
import Parser

-- Helper function that tests all files in the given directory
testAllFilesInDirectory :: FilePath -> IO ()
testAllFilesInDirectory dir = do
  files <- listDirectory dir
  forM_ files $ \file -> do
    let fullPath = dir </> file
    s <- BL.readFile fullPath
    case runAlex s parseLLVMIR of
      Left _ -> error $ "Failed to parse file: " ++ fullPath
      Right _ -> return ()

main :: IO ()
main = hspec $ do
  describe "Lexer.scanMany" $ do
    it "returns Right for valid tokens" $ do
      scanMany "loop_start: %1 define" `shouldSatisfy` isRight

    it "returns Left for invalid tokens" $ do
      scanMany "asdf" `shouldSatisfy` isLeft

  describe "Parser.parseLLVMIR" $ do
    it "parses all examples as expected" $ do
      testAllFilesInDirectory "examples"
