
module NameNormalizer (normalizeName, normalizeBlockName, normalizeOp) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.ByteString.Lazy.Char8 (ByteString)

normalizeName :: ByteString -> String
normalizeName name = "a" ++ removePunc (unpack name)

normalizeOp :: ByteString -> String
normalizeOp name = removePunc (unpack name)

normalizeBlockName :: ByteString -> String
normalizeBlockName name = "f" ++ removePunc (unpack name)

removePunc :: String -> String
removePunc = filter (`notElem` punctuation)
  where
    punctuation :: [Char]
    punctuation = ",.?!-:;\"'%@"

unpack :: LBS.ByteString -> String
unpack = LBS.unpack
