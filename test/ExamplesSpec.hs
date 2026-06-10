module ExamplesSpec (spec) where

import Test.Hspec
import qualified Data.ByteString.Lazy as BL
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import Control.Monad (forM_)

import Lexer
import Parser

parsesAllExaples :: FilePath -> IO ()
parsesAllExaples dir = do
  entries <- listDirectory dir
  -- Only the .ll files: examples/ also holds source subdirs (sources/, etc.).
  let files = filter ((== ".ll") . takeExtension) entries
  forM_ files $ \file -> do
    let fullPath = dir </> file
    s <- BL.readFile fullPath
    case runAlex s parseLLVMIR of
      Left _ -> error $ "Failed to parse file: " ++ fullPath
      Right _ -> return ()

spec :: Spec
spec = parallel $ do
  describe "Parser.parseLLVMIR" $ do
    it "parses all examples without erroring" $ do
      parsesAllExaples "examples"
