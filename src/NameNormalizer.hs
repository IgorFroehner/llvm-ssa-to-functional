
module NameNormalizer (normalizeName, normalizeBlockName, normalizeOp, normalizeGlobal, normalizeFloat) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.ByteString.Lazy.Char8 (ByteString)

normalizeName :: ByteString -> String
normalizeName name = "a" ++ removePunc (unpack name)

normalizeOp :: ByteString -> String
normalizeOp name = removePunc (unpack name)

-- | A floating-point literal's LLVM spelling is already a valid Haskell literal
-- (decimal-exponent form), so it passes through verbatim. The printer wraps it
-- in parentheses and annotates it with its type.
normalizeFloat :: ByteString -> String
normalizeFloat = unpack

-- | Normalize a global (function) name to a valid, non-colliding Haskell
-- identifier. Like 'normalizeOp' it strips punctuation, but it also escapes
-- names that are reserved in Haskell — most importantly @main@, which would
-- otherwise be forced to be @IO ()@ in the generated module. The same function
-- is used for both definitions and call sites, so the rename stays consistent.
normalizeGlobal :: ByteString -> String
normalizeGlobal name =
  let n = removePunc (unpack name)
  in if n `elem` reserved then n ++ "_" else n
  where
    reserved =
      "main" :
      [ "case", "class", "data", "default", "deriving", "do", "else"
      , "foreign", "if", "import", "in", "infix", "infixl", "infixr"
      , "instance", "let", "module", "newtype", "of", "then", "type", "where"
      ]

normalizeBlockName :: ByteString -> String
normalizeBlockName name = "f" ++ removePunc (unpack name)

-- | Strip the punctuation the lexer admits inside LLVM identifiers, leaving a
-- valid Haskell identifier body. Covers every such character: the sigils
-- (@%@\/@\@@), the dotted/dollar name chars LLVM allows in locals, globals and
-- block labels (@.@\/@$@ — e.g. @%for.cond@, @\@foo$bar@), and the label colon.
removePunc :: String -> String
removePunc = filter (`notElem` punctuation)
  where
    punctuation :: [Char]
    punctuation = ",.?!-:;\"'%@$"

unpack :: LBS.ByteString -> String
unpack = LBS.unpack
