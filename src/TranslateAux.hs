
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
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Int (Int64)

nameToString :: Ast.Name Range -> ByteString
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

uname :: Ast.Name Range -> ByteString
uname (Ast.LName _ name) = name
uname (Ast.GName _ name) = name

indent :: Int -> ByteString -> ByteString
indent level = LBS.append (LBS.replicate (fromIntegral level * 2) ' ')

indentEach :: Int -> [ByteString] -> ByteString
indentEach level input = LBS.concat $ map (indent level) input
