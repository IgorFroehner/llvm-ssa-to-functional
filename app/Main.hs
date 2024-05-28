module Main (main) where

import Lexer
import Parser (parseLLVMIR)
import GraphViz (plotGraph)
import Translate (translate)

import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)

import Dominance (buildGraph, dominances, buildGraphs, dominance)
import PrintAnf (printProgram)

output :: [String] -> String -> IO ()
output out str = case out of
  ("-o":outFile:_) -> writeFile outFile str
  _ -> putStrLn str

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("--graph-viz":file:out) -> do
      s <- BL.readFile file
      case runAlex s parseLLVMIR of
        Left err -> putStrLn err
        Right ast -> do
          let gs = buildGraphs ast
          let result = concatMap plotGraph gs
          output out result
    ("--dominance-viz":file:out) -> do
      s <- BL.readFile file
      case runAlex s parseLLVMIR of
        Left err -> putStrLn err
        Right ast -> do
          let g = buildGraphs ast
          let doms = dominances g
          let result = concatMap plotGraph doms
          output out result
    (file:out) -> do
      s <- BL.readFile file
      case runAlex s parseLLVMIR of
        Left err -> putStrLn err
        Right ast -> do
          let anf = translate ast
          output out (printProgram anf)
    [] -> putStrLn "Usage: stack run -- [--dominance-viz | --graph-viz] <file.ll> [-o <output-file>]"
