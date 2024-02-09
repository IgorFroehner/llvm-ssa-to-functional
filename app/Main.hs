module Main (main) where

import Lexer
import Parser
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
    s <- BL.readFile "examples/fib.ll"
    case runAlex s parseLLVMIR of
        Left err -> putStrLn err
        Right ast -> print ast
