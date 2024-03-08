module Main (main) where

import Lexer
import Parser
import Lib (genAst)

import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)
-- import qualified Ast

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("--ast":file:_) -> do
            s <- BL.readFile file
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> writeFile "output.dot" (genAst ast)
        (file:_) -> do
            s <- BL.readFile file
            print $ runAlex s parseLLVMIR
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> print ast
        [] -> putStrLn "Usage: stack run -- <file>"

-- goOverFunction :: [Ast.Function Range] -> String
-- goOverFunction (a:_) = goOverBlocks (extractBlocks a)
-- goOverFunction [] = ""

-- extractBlocks :: Ast.Function Range -> [Ast.BasicBlock Range]
-- extractBlocks (Ast.FunctionDef _ _ _ _ blocks) = blocks

-- goOverBlocks :: [Ast.BasicBlock Range] -> (Ast.BasicBlock Range -> a) -> [a]
-- goOverBlocks (a:x) f = f a : goOverBlocks x f
-- goOverBlocks [] _ = []
