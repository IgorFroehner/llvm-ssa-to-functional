
module NameNormalizer (normalizeName, normalizeBlockName) where

import qualified Lexer
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Ast

normalizeName :: Ast.Name Lexer.Range -> String
normalizeName (Ast.GName _ name) = removeFirst (removePunc (unpack name))
normalizeName (Ast.LName _ name) = "a" ++ removeFirst (removePunc (unpack name))

normalizeBlockName :: Ast.Name Lexer.Range -> String
normalizeBlockName (Ast.GName _ name) = "f" ++ removeLast (unpack name)
normalizeBlockName (Ast.LName _ name) = "f" ++ removeLast (unpack name)

removeFirst :: String -> String
removeFirst [] = []
removeFirst (_:x) = x

unpack :: LBS.ByteString -> String
unpack = LBS.unpack

removeLast :: String -> String
removeLast [_] = []
removeLast (x:xs) = x : removeLast xs
removeLast [] = []

removePunc :: String -> String
removePunc = filter (`notElem` punctuation)
  where
    punctuation :: [Char]
    punctuation = ",.?!-:;\"'"
