module Main (main) where

import Lexer
import Parser
import Lib

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.Environment (getArgs)
import qualified Ast

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("--ast":file:_) -> do
            s <- BL.readFile file
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> writeFile "output.dot" (goOverProgram ast)
        (file:_) -> do
            s <- BL.readFile file
            print $ runAlex s parseLLVMIR
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> print ast
        [] -> putStrLn "Usage: stack run -- <file>"
