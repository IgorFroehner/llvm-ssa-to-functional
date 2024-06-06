
module AstHelpers (getFlow, getLabel, findBlock) where

import qualified Ast
import TranslateAux
import Lexer

getLabel :: Ast.BasicBlock Range -> String
getLabel (Ast.BasicBlock _ label _ _ _) = nameToString label

getFlow :: Ast.BasicBlock Range -> Ast.Flow Range
getFlow (Ast.BasicBlock _ _ _ _ flow) = flow

findBlock :: [Ast.BasicBlock Range] -> String -> Ast.BasicBlock Range
findBlock (a:x) name = if getLabel a == name then a else findBlock x name
findBlock [] name = error ("Block not found:" ++ name)
