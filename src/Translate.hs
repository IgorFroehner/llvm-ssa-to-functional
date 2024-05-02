
module Translate (translate) where

import Lexer
import TranslateAux
import AstHelpers

import qualified Ast

import Text.Printf
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph (lab, suc, Node)
import Data.Maybe (fromJust)

-- Depth-first traversal
treeTraversal :: Gr String () -> [Ast.BasicBlock Range] -> Node -> [String]
treeTraversal gr blocks node = dfs 2 node ++ [callInitialBlock] where
  dfs depth n =
    let
      label = fromJust (lab gr n)
      block = findBlock blocks label
      nodeString = translateBlock depth block
      children = suc gr n
      childLines = concatMap (dfs (depth + 1)) children
      sufix = blockSufix depth blocks block
    in nodeString : childLines ++ [sufix]
  initialBlock = findBlock blocks (fromJust (lab gr node))
  callInitialBlock = ident 2 "in " ++ getLabel initialBlock ++ "\n"

translate :: [Ast.Function Range] -> Gr String () -> String
translate fs dom = header ++ goOverFunctions fs dom

goOverFunctions :: [Ast.Function Range] -> Gr String () -> String
goOverFunctions (f:fs) dom = translateFunction f dom ++ goOverFunctions fs dom
goOverFunctions [] _ = ""

header :: String
header = "import Data.Bits\n\n"

blockSufix :: Int -> [Ast.BasicBlock Range] -> Ast.BasicBlock Range -> String
blockSufix level blocks block = translateFlow level blocks (getFlow block) (getLabel block)

translateFunction :: Ast.Function Range -> Gr String () -> String
translateFunction (Ast.FunctionDef _ _ name args blocks) dom = functionString (nameToString name) (placeFunctionArgs args) (concat (treeTraversal dom blocks 0))
translateFunction _ _ = ""

placeFunctionArgs :: [Ast.ArgumentDef Range] -> String
placeFunctionArgs (a:x) = placeFunctionArg a ++ " " ++ placeFunctionArgs x
placeFunctionArgs [] = ""

placeFunctionArg :: Ast.ArgumentDef Range -> String
placeFunctionArg (Ast.ArgumentDef _ _ (Just name)) = nameToString name
placeFunctionArg (Ast.ArgumentDef _ _ Nothing) = "-"

translateBlock :: Int -> Ast.BasicBlock Range -> String
translateBlock level (Ast.BasicBlock _ label phis stmts _) = blockString level (nameToString label) (getArgsFromPhis phis) (translateStmts (level + 1) stmts)

getArgsFromPhis :: [Ast.PhiDec Range] -> String
getArgsFromPhis (phi:x) = getArgFromPhi phi ++ " " ++ getArgsFromPhis x
getArgsFromPhis [] = ""

getArgFromPhi :: Ast.PhiDec Range -> String
getArgFromPhi (Ast.PhiDec _ name _) = nameToString name

translateStmts :: Int -> [Ast.Stmt Range] -> String
translateStmts level = concatMap (translateStmt level)

translateStmt :: Int -> Ast.Stmt Range -> String
translateStmt level (Ast.SDec stmt) = translateDec level stmt
translateStmt level (Ast.SCall stmt) = translateCall level stmt

translateDec :: Int -> Ast.Dec Range -> String
translateDec level (Ast.DecCall _ name call) = decString level (nameToString name) (translateCallDec call)
translateDec level (Ast.DecIcmp _ name icmp) = decString level (nameToString name) (translateICMP icmp)
translateDec level (Ast.DecBinOp _ name binop) = decString level (nameToString name) (translateBinOp binop)
translateDec level (Ast.DecConvOp _ name convop) = decString level (nameToString name) (translateConvOp convop)
translateDec level (Ast.DecSelect _ name select) = decString level (nameToString name) (translateSelect select)

translateConvOp :: Ast.ConvOpCall Range -> String
translateConvOp (Ast.ConvOpCall _ _ _ value _) = unvalue value

translateCallDec :: Ast.Call Range -> String
translateCallDec (Ast.Call _ _ name args) = nameToString name ++ " " ++ translateArgs args

translateCall :: Int -> Ast.Call Range -> String
translateCall level (Ast.Call _ _ name args) = ident level "-- " ++ nameToString name ++ " " ++ translateArgs args ++ "\n"

translateArgs :: [Ast.CallArgument Range] -> String
translateArgs ((Ast.CallArgument _ _ value):x) = unvalue value ++ " " ++ translateArgs x
translateArgs [] = ""

translateFlow :: Int -> [Ast.BasicBlock Range] -> Ast.Flow Range -> String -> String
translateFlow level blocks (Ast.FlowBranch br) currentLabel = translateBranch level blocks currentLabel br
translateFlow level _ (Ast.FlowReturn ret) _ = translateReturn (level + 1) ret

translateReturn :: Int -> Ast.Return Range -> String
translateReturn level (Ast.Return _ _ (Just value)) = ident level $ returnString (unvalue value)
translateReturn level (Ast.Return _ _ Nothing) = ident level $ returnString "Nothing"

translateBranch :: Int -> [Ast.BasicBlock Range] -> String -> Ast.Br Range -> String
translateBranch l blocks currentLabel (Ast.Br _ [name]) = gotoString (l + 1) toLabel callArgs
  where
    toLabel = nameToString name
    toBlock = findBlock blocks toLabel
    callArgs = callArgumentsFromPhis toBlock currentLabel
translateBranch l blocks currentLabel (Ast.Br _ (condValue:name1:name2:_)) = brIfString (l + 1) cond toLabel1 callArgs1 toLabel2 callArgs2
  where
    cond = nameToString condValue
    toLabel1 = nameToString name1
    fromBlock1 = findBlock blocks toLabel1
    toLabel2 = nameToString name2
    fromBlock2 = findBlock blocks toLabel2
    callArgs1 = callArgumentsFromPhis fromBlock1 currentLabel
    callArgs2 = callArgumentsFromPhis fromBlock2 currentLabel
translateBranch _ _ _ _ = "Unkown branch type"

callArgumentsFromPhis :: Ast.BasicBlock Range -> String -> String
callArgumentsFromPhis (Ast.BasicBlock _ _ phis _ _) currentLabel = concatMap (getValueForCurrentLabel currentLabel) phis

getValueForCurrentLabel :: String -> Ast.PhiDec Range -> String
getValueForCurrentLabel currentLabel (Ast.PhiDec _ _ (Ast.Phi _ _ values)) = getValueForCurrentLabelFromValues values currentLabel

getValueForCurrentLabelFromValues :: [(Ast.Value Range, Ast.Name Range)] -> String -> String
getValueForCurrentLabelFromValues values currentLabel =
  case filter (\(_, name) -> nameToString name == currentLabel) values of
    [(value, _)] -> unvalue value ++ " "
    _ -> ""

ident :: Int -> String -> String
ident level str = replicate (level * 2) ' ' ++ str

identEach :: Int -> [String] -> String
identEach level = concatMap (ident level)

functionString :: String -> String -> String -> String
functionString = printf "%s %s=\n  let\n%s"

blockString :: Int -> String -> String -> String -> String -- -> String -> String -> String
blockString level = printf (identEach level ["%s %s=\n", "  let\n%s"])

decString :: Int -> String -> String -> String
decString level = printf (ident level "%s = %s\n")

returnString :: String -> String
returnString = printf "in %s\n"

brIfString :: Int -> String -> String -> String -> String -> String -> String
brIfString l = printf (identEach l ["in if %s == 1\n", "  then %s %s\n", "  else %s %s\n"])

gotoString :: Int -> String -> String -> String
gotoString l = printf (ident l "in %s %s\n")
