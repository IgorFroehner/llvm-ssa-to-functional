module Main (main) where

import Lexer
import Parser (parseLLVMIR)
import GraphViz (plotGraph)
import Translate (translate)

import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)

import Dominance (buildGraph, dominance)
import PrintAnf (printProgram)

-- runParser 

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
    (file:out) -> do
      s <- BL.readFile file
      case runAlex s parseLLVMIR of
        Left err -> putStrLn err
        Right ast -> do
          let g = buildGraph (head ast)
          let dom = dominance g
          let anf = translate ast dom
          let output = printProgram anf
          case out of
            ("-o":outFile:_) -> writeFile outFile output
            _ -> putStrLn output
    [] -> putStrLn "Usage: stack run -- [--dominance-viz | --graph-viz] <file.ll> [-o <output-file>]"
