module Main (main) where

import Lexer
import Parser
import Lib (genAst)

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
                Right ast -> writeFile "output.dot" (genAst ast)
        (file:_) -> do
            s <- BL.readFile file
            print $ runAlex s parseLLVMIR
            case runAlex s parseLLVMIR of
                Left err -> putStrLn err
                Right ast -> writeFile "output.out" (beautyPrint ast)
        [] -> putStrLn "Usage: stack run -- <file>"

unpack :: LBS.ByteString -> String
unpack = LBS.unpack

beautyPrint :: [Ast.Function Range] -> String
beautyPrint (a:_) = function a
beautyPrint [] = ""

function :: Ast.Function Range -> String
function (Ast.FunctionDef _ _ (Ast.GName _ name) _ blocks) = "F Def " ++ unpack name ++ ":\n" ++ unblocks blocks
function (Ast.FunctionDec _ _ (Ast.GName _ name) _) = "F Dec" ++ unpack name
function _ = "Unknown\n"

unblocks :: [Ast.BasicBlock Range] -> String
unblocks = concatMap unblock

unblock :: Ast.BasicBlock Range -> String
unblock (Ast.BasicBlock _ Nothing phis stmts _) = unphis phis ++ unstmts stmts
unblock (Ast.BasicBlock _ (Just name) phis stmts _) = "Block " ++ uname name ++ "\n" ++ unphis phis ++ unstmts stmts

unphis :: [Ast.PhiDec Range] -> String
unphis (a:x) = unphis x ++ unphi a
unphis [] = ""

unphi :: Ast.PhiDec Range -> String
unphi (Ast.PhiDec _ name (Ast.Phi _ _ args)) = "  Phi " ++ uname name ++ " = phi(" ++ phiargs args ++ ")\n"

unstmts :: [Ast.Stmt Range] -> String
unstmts = concatMap unstmt

unstmt :: Ast.Stmt Range -> String
unstmt (Ast.SDec stmt) = undec stmt
unstmt (Ast.SCall stmt) = uncall stmt ++ "\n"
unstmt (Ast.SReturn stmt) = "  Return " ++ unreturn stmt ++ "\n"

unreturn :: Ast.Return Range -> String
unreturn (Ast.Return _ _ (Just valueReturned)) = unvalue valueReturned
unreturn (Ast.Return _ _ Nothing) = "void"

undec :: Ast.Dec Range -> String
undec (Ast.DecCall _ name call) = "  Dec " ++ uname name ++ " = " ++ uncall call ++ "\n"
undec (Ast.DecIcmp _ name icmp) = "  Dec " ++ uname name ++ " = " ++ unicmp icmp ++ "\n"
undec (Ast.DecBinOp _ name binop) = "  Dec " ++ uname name ++ " = " ++ unbinop binop ++ "\n"
undec (Ast.DecConvOp _ name convop) = "  Dec " ++ uname name ++ " = " ++ unconvop convop ++ "\n"
undec (Ast.DecSelect _ name select) = "  Dec " ++ uname name ++ " = " ++ unselect select ++ "\n"

uncall :: Ast.Call Range -> String
uncall (Ast.Call _ _ (Ast.GName _ name) _) = "call " ++ unpack name
uncall _ = "Unknown"

unicmp :: Ast.Icmp Range -> String
unicmp (Ast.Icmp _ (Ast.Cmp _ cmp) _ value1 value2) = "icmp " ++ unpack cmp ++ " " ++ unvalue value1 ++ ", " ++ unvalue value2

unbinop :: Ast.BinOpCall Range -> String
unbinop (Ast.BinOpCall _ (Ast.BinOp _ binop) _ value1 value2) = "binop " ++ unpack binop ++ " " ++ unvalue value1 ++ ", " ++ unvalue value2

unconvop :: Ast.ConvOpCall Range -> String
unconvop (Ast.ConvOpCall _ (Ast.ConvOp _ convop) _ value _) = "convop " ++ unpack convop ++ " " ++ unvalue value

unselect :: Ast.Select Range -> String
unselect (Ast.Select _ _ value1 value2 value3) = "select " ++ unvalue value1 ++ ", " ++ unvalue value2 ++ ", " ++ unvalue value3

phiargs :: [(Ast.Value Range, Ast.Name Range)] -> String
phiargs (a:x) = phiargs x ++ phiarg a
phiargs [] = ""

phiarg :: (Ast.Value Range, Ast.Name Range) -> String
phiarg (value, name) = "(" ++ uname name ++ ", " ++ unvalue value ++ ")"

unvalue :: Ast.Value Range -> String
unvalue (Ast.ValueInt (Ast.IntegerValue _ int)) = show int
unvalue (Ast.ValueName name) = uname name

uname :: Ast.Name Range -> String
uname (Ast.LName _ name) = unpack name
uname (Ast.GName _ name) = unpack name
