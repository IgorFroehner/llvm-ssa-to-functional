
module TranslateAux (translateICMP, translateBinOp, translateSelect, unpack, unvalue, uname) where

import qualified Ast
import Lexer

import qualified Data.ByteString.Lazy.Char8 as LBS
import Text.Printf

translateICMP :: Ast.Icmp Range -> String
translateICMP (Ast.Icmp _ cmp _ a b) = printf "if %s %s %s then 1 else 0" (unvalue a) (acmp cmp) (unvalue b)

acmp :: Ast.Cmp Range -> String
acmp (Ast.Cmp _ cmp) = case cmp of
  "eq" -> "=="
  "ne" -> "/="
  "ugt" -> ">"
  "uge" -> ">="
  "ult" -> "<"
  "ule" -> "<="
  "sgt" -> ">"
  "sge" -> ">="
  "slt" -> "<"
  "sle" -> "<="
  _ -> "UNKNOWN CMP"

translateBinOp :: Ast.BinOpCall Range -> String
translateBinOp (Ast.BinOpCall _ binop _ a b) = unvalue a ++ abinop binop ++ unvalue b

-- Select a (Type a) (Value a) (Value a) (Value a)
translateSelect :: Ast.Select Range -> String
translateSelect (Ast.Select _ _ a b c) = printf "if %s /= 0 then %s else %s" (unvalue a) (unvalue b) (unvalue c)

abinop :: Ast.BinOp Range -> String
abinop (Ast.BinOp _ op) = case op of
  "add" -> " + "
  "sub" -> " - "
  "mul" -> " * "
  "udiv" -> " `div` "
  "sdiv" -> " `div` "
  "urem" -> " `mod` "
  "srem" -> " `mod` "
  "and" -> " .&. "
  "or" -> " .|. "
  _ -> op

unpack :: LBS.ByteString -> String
unpack = LBS.unpack

unvalue :: Ast.Value Range -> String
unvalue (Ast.ValueInt (Ast.IntegerValue _ int)) = if int >= 0 then show int else "(" ++ show int ++ ")"
unvalue (Ast.ValueName name) = uname name

uname :: Ast.Name Range -> String
uname (Ast.LName _ name) = name
uname (Ast.GName _ name) = name
