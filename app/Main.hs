module Main (main) where

import Lexer
import Parser (parseLLVMIR)
import GraphViz (plotGraph)
import BeatyPrint (beautyPrint)
import Translate (translate)

import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)

import Dominance (buildGraph, dominance)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("--graph-viz":file:out) -> do
            s <- BL.readFile file
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> do
                    let g = buildGraph (head ast)
                    case out of
                        ("-o":outFile:_) -> writeFile outFile (plotGraph g)
                        _ -> putStrLn (plotGraph g)
        ("--dominance-viz":file:out) -> do
            s <- BL.readFile file
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> do
                    let g = buildGraph (head ast)
                    let dom = dominance g
                    case out of
                        ("-o":outFile:_) -> writeFile outFile (plotGraph dom)
                        _ -> putStrLn (plotGraph dom)
        ("--beauty":file:out) -> do
            s <- BL.readFile file
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> do
                    case out of
                        ("-o":outFile:_) -> writeFile outFile (beautyPrint ast)
                        _ -> putStrLn (beautyPrint ast)
        (file:out) -> do
            s <- BL.readFile file
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> do
                    let dom = dominance (buildGraph (head ast))
                    case out of
                        ("-o":outFile:_) -> writeFile outFile (translate ast dom)
                        _ -> putStrLn (translate ast dom)
        [] -> putStrLn "Usage: stack run -- [--dominance-viz | --graph-viz | --beauty] <file.ll> [-o <output-file>]"
