
module NameNormalizer (normalizeName) where

import Lexer
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Ast

normalizeName :: Ast.Name Range -> String
normalizeName (Ast.GName _ name) = removeFirst (unpack name)
normalizeName (Ast.LName _ name) = "a" ++ removeFirst (unpack name)

removeFirst :: String -> String
removeFirst [] = []
removeFirst (_:x) = x

unpack :: LBS.ByteString -> String
unpack = LBS.unpack
