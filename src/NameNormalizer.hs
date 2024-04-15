
module NameNormalizer (normalizeName, normalizeBlockName) where

import qualified Lexer
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Ast

normalizeName :: Ast.Name Lexer.Range -> String
normalizeName (Ast.GName _ name) = removePunc (unpack name)
normalizeName (Ast.LName _ name) = "a" ++ removePunc (unpack name)

normalizeBlockName :: Ast.Name Lexer.Range -> String
normalizeBlockName (Ast.GName _ name) = "f" ++ removePunc (unpack name)
normalizeBlockName (Ast.LName _ name) = "f" ++ removePunc (unpack name)

removePunc :: String -> String
removePunc = filter (`notElem` punctuation)
  where
    punctuation :: [Char]
    punctuation = ",.?!-:;\"'%@"

unpack :: LBS.ByteString -> String
unpack = LBS.unpack
