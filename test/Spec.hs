import Test.Hspec
import Data.Either (isRight, isLeft)
import qualified Data.ByteString.Lazy as BL
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Control.Monad (forM_)

import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph (mkGraph)

import Lexer
import Parser
import qualified Ast
import qualified Anf

import Translate (translate)

parsesAllExaples :: FilePath -> IO ()
parsesAllExaples dir = do
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
      parsesAllExaples "examples"
