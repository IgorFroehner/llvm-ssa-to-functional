module Main (main) where

import Lexer
import Parser
import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: llvm-parser <file>"
        (file:_) -> do
            s <- BL.readFile file
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> print ast
