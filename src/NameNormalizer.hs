
module NameNormalizer (normalizeName, normalizeBlockName, normalizeOp) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.ByteString.Lazy.Char8 (ByteString)

normalizeName :: ByteString -> ByteString
normalizeName name = LBS.cons 'a' (removePunc name)

normalizeOp :: ByteString -> ByteString
normalizeOp = removePunc

normalizeBlockName :: ByteString -> ByteString
normalizeBlockName name = LBS.cons 'f' (removePunc name)

removePunc :: ByteString -> ByteString
removePunc = LBS.filter (`notElem` punctuation)
  where
    punctuation :: [Char]
    punctuation = ",.?!-:;\"'%@"
