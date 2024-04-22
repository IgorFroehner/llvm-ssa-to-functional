module Main (main) where

import Lexer
import Parser (parseLLVMIR)
-- import GraphViz (genAst)
import BeatyPrint (beautyPrint)
import Translate (translate)

import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)

import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.Dominators (iDom)
import Dominance (buildGraph)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("--graph":file:_) -> do
            s <- BL.readFile file
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> do
                    let g = buildGraph (head ast)
                    print $ show g
        ("--beauty":file:_) -> do
            s <- BL.readFile file
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> putStrLn (beautyPrint ast)
                -- Right ast -> writeFile "beauty.out" (beautyPrint ast)
        (file:_) -> do
            s <- BL.readFile file
            -- print $ runAlex s parseLLVMIR
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> putStrLn (translate ast)
        [] -> putStrLn "Usage: stack run -- <file>"
