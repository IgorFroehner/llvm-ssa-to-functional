
module TranslateAux (
  translateOperator,
  translateCmpType,
  unpack,
  uname,
  nameToString,
  indent,
  indentEach
  ) where

import qualified Ast
import Lexer

import qualified Data.ByteString.Lazy.Char8 as LBS

nameToString :: Ast.Name Range -> String
nameToString (Ast.GName _ name) = name
nameToString (Ast.LName _ name) = name

translateCmpType :: String -> String
translateCmpType str = case str of
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

translateOperator :: String -> String
translateOperator str = case str of
  "add" -> " + "
  "sub" -> " - "
  "mul" -> " * "
  "udiv" -> " `div` "
  "sdiv" -> " `div` "
  "urem" -> " `mod` "
  "srem" -> " `mod` "
  "and" -> " .&. "
  "or" -> " .|. "
  "xor" -> " `xor` "
  "shl" -> " `shiftL` "
  "lshr" -> " `shiftR` "
  _ -> "UNKNOWN OP"

unpack :: LBS.ByteString -> String
unpack = LBS.unpack

uname :: Ast.Name Range -> String
uname (Ast.LName _ name) = name
uname (Ast.GName _ name) = name

indent :: Int -> String -> String
indent level str = replicate (level * 2) ' ' ++ str

indentEach :: Int -> [String] -> String
indentEach level = concatMap (indent level)
