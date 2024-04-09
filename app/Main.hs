module Main (main) where

import Lexer
import Parser ( parseLLVMIR )
import GraphViz (genAst)
import BeatyPrint (beautyPrint)

import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("--graph":file:_) -> do
            s <- BL.readFile file
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> writeFile "ast.dot" (genAst ast)
        ("--beauty":file:_) -> do
            s <- BL.readFile file
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> writeFile "beauty.out" (beautyPrint ast)
        (file:_) -> do
            s <- BL.readFile file
            print $ runAlex s parseLLVMIR
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> print ast
        [] -> putStrLn "Usage: stack run -- <file>"
