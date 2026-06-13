
module TranslateAux (
  translateOperator,
  translateCmpType,
  unpack,
  uname,
  nameToString,
  indent,
  indentEach,
  llvmIntWidth,
  hsTypeOfLlvm,
  widthToHsType,
  widthToWordType
  ) where

import qualified Ast
import Lexer

import Data.Char (isDigit)

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
  "sdiv" -> " `quot` "
  "urem" -> " `mod` "
  "srem" -> " `rem` "
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

-- | Parse the bit-width of an LLVM integer type spelling (@"i32"@ -> @32@).
-- Returns 'Nothing' for non-integer types (@void@, @ptr@, …), which the
-- supported subset never does arithmetic on.
integerWidth :: String -> Maybe Int
integerWidth s = case dropWhile (/= 'i') s of
  ('i':ds) | not (null ds) && all isDigit ds -> Just (read ds)
  _ -> Nothing

-- | Like 'integerWidth' but partial — for positions the subset guarantees to be
-- integer (operands of conv ops, arguments).
llvmIntWidth :: String -> Int
llvmIntWidth s = case integerWidth s of
  Just n  -> n
  Nothing -> error ("expected an integer type, got: " ++ s)

-- | The Haskell type a value of the given bit-width is represented by. Widths
-- round *up* to the next available 'Data.Int' size, so e.g. the @i33@ that
-- appears in @sum.ll@ becomes 'Int64' and computes without overflow before its
-- @trunc@ back to @i32@.
widthToHsType :: Int -> String
widthToHsType n
  | n <= 8    = "Int8"
  | n <= 16   = "Int16"
  | n <= 32   = "Int32"
  | n <= 64   = "Int64"
  | otherwise = error ("unsupported integer width: i" ++ show n)

-- | The unsigned counterpart of 'widthToHsType', used to make @zext@
-- zero-extend rather than sign-extend.
widthToWordType :: Int -> String
widthToWordType n = "Word" ++ drop 3 (widthToHsType n)

-- | The Haskell type for an LLVM type spelling. Non-integer (only @void@ in the
-- subset, where the translation fabricates a @ret 0@) falls back to 'Int'.
hsTypeOfLlvm :: String -> String
hsTypeOfLlvm s = maybe "Int" widthToHsType (integerWidth s)

indent :: Int -> String -> String
indent level str = replicate (level * 2) ' ' ++ str

indentEach :: Int -> [String] -> String
indentEach level = concatMap (indent level)
