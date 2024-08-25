module ExamplesSpec (spec) where

import Test.Hspec
import qualified Data.ByteString.Lazy as BL
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Control.Monad (forM_)

import Lexer
import Parser

parsesAllExaples :: FilePath -> IO ()
parsesAllExaples dir = do
  files <- listDirectory dir
  forM_ files $ \file -> do
    let fullPath = dir </> file
    s <- BL.readFile fullPath
    case runAlex s parseLLVMIR of
      Left _ -> error $ "Failed to parse file: " ++ fullPath
      Right _ -> return ()

spec :: Spec
spec = parallel $ do
  describe "Parser.parseLLVMIR" $ do
    it "parses all examples as expected" $ do
      parsesAllExaples "examples"
