module Main (main) where

import Lexer
import Parser

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.Environment (getArgs)
import qualified Ast

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("--ast":file:_) -> do
            s <- BL.readFile file
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> writeFile "output.dot" (goOverProgram ast)
        (file:_) -> do
            s <- BL.readFile file
            print $ runAlex s parseLLVMIR
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> print ast
        [] -> putStrLn "Usage: stack run -- <file>"

goOverProgram :: [Ast.Function Range] -> String
goOverProgram (a:_) = functions a
goOverProgram [] = ""

functions :: Ast.Function Range -> String
functions (Ast.FunctionDef _ _ (Ast.GName _ name) _ blocks) = digraph (function (LBS.unpack name) blocks)
functions (Ast.FunctionDec _ _ (Ast.GName _ name) _) = show name
functions _ = "Unknown"

function :: String -> [Ast.BasicBlock Range] -> String
function name blocks = pointFunction ("FuncDef " ++ name) blocks

digraph :: String -> String
digraph content = "digraph {\n" ++ content ++ "}\n"

pointFunction :: String -> [Ast.BasicBlock Range] -> String
pointFunction name blocks = concatMap (\stmt -> parentNode name ++ stmt) (map statements (concatMap extractStatements blocks))

extractStatements :: Ast.BasicBlock a -> [Ast.Stmt a]
extractStatements (Ast.BasicBlock _ _ stmts) = stmts

parentNode :: String -> String
parentNode name = " \"" ++ name ++ "\" -> "

statements :: Ast.Stmt Range -> String
statements (Ast.SDec stmt) = parentNode "SDec" ++ decs stmt ++ "\n"
statements (Ast.SCall stmt) = parentNode "SCall" ++ calls stmt ++ "\n"
statements (Ast.SReturn stmt) = parentNode "SReturn" ++ returns stmt ++ "\n"
statements (Ast.SBr stmt) = brs stmt ++ "\n"

decs :: Ast.Dec Range -> String
decs (Ast.DecCall _ (Ast.LName _ name) call) = receiveBlock [show ("LName " ++ show name), calls call]
decs _ = "Unknown"

receiveBlock :: [String] -> String
receiveBlock x = "{" ++ receiveList x ++ "}"

receiveList :: [String] -> String
receiveList (a:x) = a ++ " " ++ receiveList x
receiveList [] = ""

calls :: Ast.Call Range -> String
calls (Ast.Call _ _ (Ast.GName _ name) _) = "\"call " ++ LBS.unpack name ++ "\""
calls _ = "Unknown"

returns :: Ast.Return Range -> String
returns (Ast.Return _ _ (Just valueReturned)) = show (value valueReturned)
returns (Ast.Return _ _ Nothing) = "void"

brs :: Ast.Br Range -> String
brs (Ast.Br _ _) = "br"

value :: Ast.Value Range -> String
value (Ast.ValueInt (Ast.IntegerValue _ int)) = "ValueInt" ++ show int
value (Ast.ValueName (Ast.LName _ name)) = "ValueName " ++ show name
value (Ast.ValueName (Ast.GName _ name)) = "Unknown"
