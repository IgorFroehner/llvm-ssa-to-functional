module Main (main) where

import Lexer
import Parser (parseLLVMIR)
import GraphViz (plotGraph)
import Translate (translate)

import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)

import Dominance (buildGraph, dominance)
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
          let g = buildGraph (head ast)
          output out (plotGraph g)
    ("--dominance-viz":file:out) -> do
      s <- BL.readFile file
      case runAlex s parseLLVMIR of
        Left err -> putStrLn err
        Right ast -> do
          let g = buildGraph (head ast)
          let dom = dominance g
          output out (plotGraph dom)
    (file:out) -> do
      s <- BL.readFile file
      case runAlex s parseLLVMIR of
        Left err -> putStrLn err
        Right ast -> do
          let anf = translate ast
          output out (printProgram anf)
    [] -> putStrLn "Usage: stack run -- [--dominance-viz | --graph-viz] <file.ll> [-o <output-file>]"
