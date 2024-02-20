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
            -- Handle the case when '--ast' option is provided
            s <- BL.readFile file
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> print (goOverProgram ast)
        (file:_) -> do
            s <- BL.readFile file
            print $ runAlex s parseLLVMIR
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> print ast
        [] -> putStrLn "Usage: stack run -- <file>"

goOverProgram :: [Ast.Function Range] -> String
goOverProgram (a:_) = ""
-- goOverProgram (a:_) = functions a

-- functions :: Ast.Function Range -> String
-- functions (Ast.FunctionDef _ _ (Ast.GName _ name) _ blocks) = "digraph {\n" ++ pointFunction ("FuncDef " ++ LBS.unpack name) blocks ++ "}\n"
-- functions (Ast.FunctionDec _ _ (Ast.GName _ name) _) = show name
-- functions _ = "Unknown"

-- pointFunction :: String -> [Ast.BasicBlock Range] -> String
-- pointFunction name (Ast.BasicBlock _ _ stmts) = parentNode name ++ concatMap unbuildBlock stmts
-- pointFunction _ a = ""

-- unbuildBlock :: [Ast.Stmt Range] -> String
-- unbuildBlock (a:x) = statements a ++ unbuildBlock x
-- unbuildBlock [] = ""

-- parentNode :: String -> String
-- parentNode name = " \"" ++ name ++ "\" -> "

-- statements :: Ast.Stmt Range -> String
-- statements (Ast.SDec stmt) = parentNode "SDec" ++ decs stmt ++ "\n"
-- statements (Ast.SCall stmt) = parentNode "SCall" ++ calls stmt ++ "\n"
-- statements (Ast.SReturn stmt) = parentNode "SReturn" ++ returns stmt ++ "\n"
-- statements (Ast.SBr stmt) = brs stmt ++ "\n"

-- decs :: Ast.Dec Range -> String
-- decs (Ast.DecCall _ (Ast.LName _ name) call) = receiveBlock [show ("LName " ++ show name), calls call]
-- decs _ = "Unknown"

-- receiveBlock :: [String] -> String
-- receiveBlock x = "{" ++ receiveList x ++ "}"

-- receiveList :: [String] -> String
-- receiveList (a:x) = a ++ " " ++ receiveList x
-- receiveList [] = ""

-- calls :: Ast.Call Range -> String
-- calls (Ast.Call _ _ (Ast.GName _ name) _) = "\"call " ++ LBS.unpack name ++ "\""

-- returns :: Ast.Return Range -> String
-- returns (Ast.Return _ _ (Just valueReturned)) = show (value valueReturned)

-- brs :: Ast.Br Range -> String
-- brs (Ast.Br _ cmpValue _ _ _ _) = "br " ++ value cmpValue

-- value :: Ast.Value Range -> String
-- value (Ast.ValueInt (Ast.IntegerValue _ int)) = "ValueInt" ++ show int
-- value (Ast.ValueName (Ast.LName _ name)) = "ValueName " ++ show name
