
module AstHelpers (getFlow, getLabel) where

import qualified Ast
import TranslateAux
import Lexer

import Data.Maybe (fromJust)

getLabel :: Ast.BasicBlock Range -> String
getLabel (Ast.BasicBlock _ label _ _ _) = nameToString label

getFlow :: Ast.BasicBlock Range -> Ast.Flow Range
getFlow (Ast.BasicBlock _ _ _ _ flow) = fromJust flow
