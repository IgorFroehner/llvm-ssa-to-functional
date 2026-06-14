module Main (main) where

import Lexer
import Parser (parseLLVMIR)
import GraphViz (plotGraph)
import Translate (translate)

import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)

import Dominance (dominances, buildGraphs)
import Effect (annotate)
import Backend (Backend, runBackend)
import PrintAnf (haskellBackend)
import AnnotDump (annotDumpBackend)

-- | The output backends selectable with @--backend NAME@. The Haskell source
-- printer is the default and first; 'annotDumpBackend' is the annotation-reading
-- debug backend that proves the backend seam (see
-- docs/roadmap/plans/04-annotated-anf-ast.md).
backends :: [Backend]
backends = [haskellBackend, annotDumpBackend]

output :: [String] -> String -> IO ()
output out str = case out of
  ("-o":outFile:_) -> writeFile outFile str
  _ -> putStrLn str

-- | Run the translation pipeline (parse → translate → annotate → render) with
-- the chosen backend, writing to the requested sink.
translateWith :: String -> FilePath -> [String] -> IO ()
translateWith backendId file out = do
  s <- BL.readFile file
  case runAlex s parseLLVMIR of
    Left err -> putStrLn err
    Right ast ->
      case runBackend backends backendId (annotate (translate ast)) of
        Left unknown -> putStrLn ("Unknown backend: " ++ unknown)
        Right result -> output out result

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
    ("--backend":backendId:file:out) -> translateWith backendId file out
    (file:out) -> translateWith "haskell" file out
    [] -> putStrLn "Usage: stack run -- [--dominance-viz | --graph-viz | --backend NAME] <file.ll> [-o <output-file>]"
