module Lib (genAst) where

import Lexer

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Ast

genAst :: [Ast.Function Range] -> String
genAst (a:_) = functions a
genAst [] = ""

functions :: Ast.Function Range -> String
functions (Ast.FunctionDef _ _ (Ast.GName _ name) _ blocks) = digraph (function (LBS.unpack name) blocks)
functions (Ast.FunctionDec _ _ (Ast.GName _ name) _) = show name
functions _ = "Unknown"

function :: String -> [Ast.BasicBlock Range] -> String
function name = pointFunction ("FuncDef " ++ name)

digraph :: String -> String
digraph content = "digraph {\n" ++ content ++ "}\n"

pointFunction :: String -> [Ast.BasicBlock Range] -> String
pointFunction name blocks = concatMap ((\stmt -> parentNode name ++ stmt) . statements) (concatMap extractStatements blocks)

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
value (Ast.ValueName (Ast.GName _ _)) = "Unknown"
