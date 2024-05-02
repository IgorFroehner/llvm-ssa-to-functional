module Main (main) where

import Lexer
import Parser (parseLLVMIR)
import GraphViz (plotGraph)
-- import BeatyPrint (beautyPrint)
import Translate (translate)

import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)

import Dominance (buildGraph, dominance)
import Anf
import ToAnf (buildAnf)
import PrintAnf (printProgram)

-- runParser 

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("--direct":file:_) -> do
            s <- BL.readFile file
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> do
                    let g = buildGraph (head ast)
                    let dom = dominance g
                    -- let anf = buildAnf ast dom
                    putStrLn $ translate ast dom
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
        (file:out) -> do
            s <- BL.readFile file
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> do
                    let g = buildGraph (head ast)
                    let dom = dominance g
                    let anf = buildAnf ast dom
                    let output = printProgram anf
                    case out of
                        ("-o":outFile:_) -> writeFile outFile output
                        _ -> putStrLn output
        [] -> putStrLn "Usage: stack run -- [--dominance-viz | --graph-viz | --beauty] <file.ll> [-o <output-file>]"
