
module AstHelpers (getFlow, getLabel, findBlock) where

import qualified Ast
import TranslateAux
import Lexer

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.ByteString.Lazy.Char8 (ByteString)

getLabel :: Ast.BasicBlock Range -> ByteString
getLabel (Ast.BasicBlock _ label _ _ _) = nameToString label

getFlow :: Ast.BasicBlock Range -> Ast.Flow Range
getFlow (Ast.BasicBlock _ _ _ _ flow) = flow

findBlock :: [Ast.BasicBlock Range] -> ByteString -> Ast.BasicBlock Range
findBlock (a:x) name = if getLabel a == name then a else findBlock x name
findBlock [] name = error ("Block not found:" ++ LBS.unpack name)
