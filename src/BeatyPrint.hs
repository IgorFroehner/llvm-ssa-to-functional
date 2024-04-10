module BeatyPrint (beautyPrint) where

import Lexer

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Ast

unpack :: LBS.ByteString -> String
unpack = LBS.unpack

beautyPrint :: [Ast.Function Range] -> String
beautyPrint = concatMap function

function :: Ast.Function Range -> String
function (Ast.FunctionDef _ _ (Ast.GName _ name) args blocks) = "F Def " ++ unpack name ++ "(" ++ unargs args ++ "):\n" ++ unblocks blocks
function (Ast.FunctionDec _ _ (Ast.GName _ name) args) = "F Dec" ++ unpack name ++ "(" ++ unargs args ++ "):\n"
function _ = "Unknown\n"

unargs :: [Ast.ArgumentDef Range] -> String
unargs = concatMap unarg

-- ArgumentDef a (Type a) (Maybe (Name a))
unarg :: Ast.ArgumentDef Range -> String
unarg (Ast.ArgumentDef _ _ (Just (Ast.LName _ name))) = unpack name
unarg (Ast.ArgumentDef _ _ (Just (Ast.GName _ name))) = unpack name
unarg (Ast.ArgumentDef _ _ Nothing) = "a"

unblocks :: [Ast.BasicBlock Range] -> String
unblocks = concatMap unblock

unblock :: Ast.BasicBlock Range -> String
unblock (Ast.BasicBlock _ Nothing phis stmts flow) = unphis phis ++ unstmts stmts ++ unflow flow
unblock (Ast.BasicBlock _ (Just name) phis stmts flow) = "Block " ++ uname name ++ "\n" ++ unphis phis ++ unstmts stmts ++ unflow flow

unflow :: Maybe (Ast.Flow Range) -> String
unflow (Just (Ast.FlowBranch (Ast.Br _ args))) = "  Br " ++ brargs args ++ "\n"
unflow (Just (Ast.FlowReturn ret)) = unreturn ret ++ "\n"
unflow Nothing = ""

brargs :: [(Ast.Type Range, Ast.Value Range)] -> String
brargs [(_,b)] = unvalue b
brargs ((_,b):x) = unvalue b ++ ", " ++ brargs x
brargs [] = ""

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
-- unstmt (Ast.SReturn stmt) = "  Return " ++ unreturn stmt ++ "\n"

unreturn :: Ast.Return Range -> String
unreturn (Ast.Return _ _ (Just valueReturned)) = "  Return " ++ unvalue valueReturned
unreturn (Ast.Return _ _ Nothing) = "  Return void"

undec :: Ast.Dec Range -> String
undec (Ast.DecCall _ name call) = "  Dec " ++ uname name ++ " = " ++ uncall call ++ "\n"
undec (Ast.DecIcmp _ name icmp) = "  Dec " ++ uname name ++ " = " ++ unicmp icmp ++ "\n"
undec (Ast.DecBinOp _ name binop) = "  Dec " ++ uname name ++ " = " ++ unbinop binop ++ "\n"
undec (Ast.DecConvOp _ name convop) = "  Dec " ++ uname name ++ " = " ++ unconvop convop ++ "\n"
undec (Ast.DecSelect _ name select) = "  Dec " ++ uname name ++ " = " ++ unselect select ++ "\n"

-- Call a (Type a) (Name a) [CallArgument a]
uncall :: Ast.Call Range -> String
uncall (Ast.Call _ _ (Ast.GName _ name) args) = "  Call " ++ unpack name ++ "(" ++ callargs args ++ ")"
uncall _ = "Unknown"

callargs :: [Ast.CallArgument Range] -> String
callargs (a:x) = callargs x ++ callarg a
callargs [] = ""

-- CallArgument a (Type a) (Value a)
callarg :: Ast.CallArgument Range -> String
callarg (Ast.CallArgument _ _ value) = unvalue value

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
