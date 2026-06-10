
module NameNormalizer (normalizeName, normalizeBlockName, normalizeOp, normalizeGlobal) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.ByteString.Lazy.Char8 (ByteString)

normalizeName :: ByteString -> String
normalizeName name = "a" ++ removePunc (unpack name)

normalizeOp :: ByteString -> String
normalizeOp name = removePunc (unpack name)

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

removePunc :: String -> String
removePunc = filter (`notElem` punctuation)
  where
    punctuation :: [Char]
    punctuation = ",.?!-:;\"'%@"

unpack :: LBS.ByteString -> String
unpack = LBS.unpack
