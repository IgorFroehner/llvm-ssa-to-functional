
module TranslateAux (translateICMP, translateBinOp, unpack, unvalue, uname) where

import Lexer

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Ast

-- Icmp a (Cmp a) (Type a) (Value a) (Value a)
translateICMP :: Ast.Icmp Range -> String
translateICMP (Ast.Icmp _ _ _ a b) = "if " ++ unvalue a ++ " == " ++ unvalue b ++ " then 1 else 0"

-- BinOpCall a (BinOp a) (Type a) (Value a) (Value a)
translateBinOp :: Ast.BinOpCall Range -> String
translateBinOp (Ast.BinOpCall _ binop _ a b) = unvalue a ++ abinop binop ++ unvalue b

abinop :: Ast.BinOp Range -> String
abinop (Ast.BinOp _ op) = case LBS.unpack op of
  "add" -> " + "
  "sub" -> " - "
  "mul" -> " * "
  "udiv" -> " `div` "
  "sdiv" -> " `div` "
  "urem" -> " `mod` "
  "srem" -> " `mod` "
  _ -> "Unknown"

unpack :: LBS.ByteString -> String
unpack = LBS.unpack

unvalue :: Ast.Value Range -> String
unvalue (Ast.ValueInt (Ast.IntegerValue _ int)) = if int >= 0 then show int else "(" ++ show int ++ ")"
unvalue (Ast.ValueName name) = uname name

uname :: Ast.Name Range -> String
uname (Ast.LName _ name) = unpack name
uname (Ast.GName _ name) = unpack name
