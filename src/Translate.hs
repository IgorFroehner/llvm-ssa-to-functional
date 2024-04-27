
module Translate (translate) where

import Lexer

import qualified Ast
import TranslateAux
import Text.Printf

import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph (lab, suc, Node)
import Data.Maybe (fromJust)

stringfyNode :: Int -> Ast.BasicBlock Range -> String
stringfyNode = translateBlock

blockSufix :: Int -> Ast.BasicBlock Range -> String
blockSufix level block = ident level ++ "in " ++ getLabel block ++ "\n"

-- Depth-first traversal
printTree :: Gr String () -> [Ast.BasicBlock Range] -> Node -> [String]
printTree gr blocks node = dfs 2 node ++ [callInitialBlock] where
  dfs depth n =
    let
      label = fromJust (lab gr n)
      block = findBlock blocks label
      nodeString = stringfyNode depth block
      children = suc gr n
      childLines = concatMap (dfs (depth + 1)) children
      -- sufix = blockSufix depth block
    in nodeString : childLines -- ++ [sufix]
  callInitialBlock = blockSufix 1 (findBlock blocks (fromJust (lab gr node)))

translate :: [Ast.Function Range] -> Gr String () -> String
translate (f:fs) dom = translateFunction f dom ++ translate fs dom
translate [] _ = ""

translateFunction :: Ast.Function Range -> Gr String () -> String
translateFunction (Ast.FunctionDef _ _ name args blocks) dom = functionString (nameToString name) (placeFunctionArgs args) (concat (printTree dom blocks 0))
translateFunction _ _ = ""

placeFunctionArgs :: [Ast.ArgumentDef Range] -> String
placeFunctionArgs (a:x) = placeFunctionArg a ++ " " ++ placeFunctionArgs x
placeFunctionArgs [] = ""

placeFunctionArg :: Ast.ArgumentDef Range -> String
placeFunctionArg (Ast.ArgumentDef _ _ (Just name)) = nameToString name
placeFunctionArg (Ast.ArgumentDef _ _ Nothing) = "-"

translateBlock :: Int -> Ast.BasicBlock Range -> String
translateBlock level (Ast.BasicBlock _ label phis stmts (Just flow)) = blockString level (nameToString label) (getArgsFromPhis phis) (translateStmts (level + 1) stmts) -- "" (translateFlow flow) (nameToString label)
translateBlock level (Ast.BasicBlock _ label phis stmts Nothing) = blockString level (nameToString label) (getArgsFromPhis phis) (translateStmts (level + 1) stmts) -- "" "" (nameToString label)

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
translateCall level (Ast.Call _ _ name args) = ident level ++ nameToString name ++ " " ++ translateArgs args ++ "\n"

translateArgs :: [Ast.CallArgument Range] -> String
translateArgs ((Ast.CallArgument _ _ value):x) = unvalue value ++ " " ++ translateArgs x
translateArgs [] = ""

translateFlow :: Ast.Flow Range -> String
translateFlow (Ast.FlowBranch br) = translateBranch br
translateFlow (Ast.FlowReturn ret) = translateReturn ret

translateReturn :: Ast.Return Range -> String
translateReturn (Ast.Return _ _ (Just value)) = returnString (unvalue value)
translateReturn (Ast.Return _ _ Nothing) = returnString "0"

functionString :: String -> String -> String -> String
functionString = printf "%s %s=\n  let\n%s"

blockString :: Int -> String -> String -> String -> String -- -> String -> String -> String
blockString level = printf ((ident level ++ "%s %s=\n") ++
                            (ident level ++ "  let\n%s"))
                    --  \%s%s%s\
                     -- \  in %s\n"

ident :: Int -> String
ident level = replicate (level * 2) ' '

decString :: Int -> String -> String -> String
decString level = printf (ident level ++ "%s = %s\n")

returnString :: String -> String
returnString = printf "in %s\n"

brIfString :: String -> String -> String -> String
brIfString = printf "in if %s == 1\n\
                    \  then %s\n\
                    \  else %s\n"

translateBranch :: Ast.Br Range -> String
translateBranch (Ast.Br _ [name]) = printf "in %s\n" (nameToString name)
translateBranch (Ast.Br _ (cond:name1:name2:_)) = brIfString (nameToString cond) (nameToString name1) (nameToString name2)
translateBranch _ = "Unkown branch type"

findBlock :: [Ast.BasicBlock Range] -> String -> Ast.BasicBlock Range
findBlock (a:x) name = if getLabel a == name then a else findBlock x name
findBlock [] name = error (printf "Block %s not found" name)

getLabel :: Ast.BasicBlock Range -> String
getLabel (Ast.BasicBlock _ label _ _ _) = nameToString label

nameToString :: Ast.Name Range -> String
nameToString (Ast.GName _ name) = name
nameToString (Ast.LName _ name) = name
