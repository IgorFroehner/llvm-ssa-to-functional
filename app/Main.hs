module Main (main) where

import Lexer
import Parser (parseLLVMIR)
import GraphViz (plotGraph)
import BeatyPrint (beautyPrint)
import Translate (translate)

import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)

import Data.Graph.Inductive.Graph (Node, lab, suc)
import Data.Graph.Inductive.PatriciaTree
import Dominance (buildGraph, dominance)
import Data.Maybe (fromJust, fromMaybe, listToMaybe)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("--dfs":file:_) -> do
            s <- BL.readFile file
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> do
                    let g = buildGraph (head ast)
                    let dom = dominance g
                    mapM_ putStrLn (printTree dom 0)
        ("--graph-viz":file:_) -> do
            s <- BL.readFile file
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> do
                    let g = buildGraph (head ast)
                    writeFile "control-graph.dot" (plotGraph g)
        ("--dominance-viz":file:_) -> do
            s <- BL.readFile file
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> do
                    let g = buildGraph (head ast)
                    let dom = dominance g
                    writeFile "dominance.dot" (plotGraph dom)
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
                Right ast -> do
                    let dom = dominance (buildGraph (head ast))
                    putStrLn (translate ast dom)
        [] -> putStrLn "Usage: stack run -- <file>"

-- Depth-first traversal and formatting
printTree :: Gr String () -> Node -> [String]
printTree gr = go 1 where
  go depth n =
    let label = fromMaybe "Unknown" (lab gr n)
        prefix = replicate (depth * 4) ' '  -- Increase the number by 4 spaces for each depth level
        children = suc gr n
        childLines = concatMap (go (depth + 1)) children
    in (prefix ++ label) : childLines
