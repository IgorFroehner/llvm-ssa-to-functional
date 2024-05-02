
module Translate (translate) where

import Lexer

import qualified Ast
import qualified Anf

import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph (lab, suc, Node)
import Data.Maybe (fromJust)

import TranslateAux
import AstHelpers

translate :: [Ast.Function Range] -> Gr String () -> Anf.Program
translate fs gr = Anf.Program (map (buildAnfFromFunction gr) fs)

buildAnfFromFunction :: Gr String () -> Ast.Function Range -> Anf.Function
buildAnfFromFunction dom (Ast.FunctionDef _ _ name args blocks) = Anf.Function (nameToString name) (anfArgs args) (anfFromTree dom blocks 0)
buildAnfFromFunction _ _ = undefined

anfFromTree :: Gr String () -> [Ast.BasicBlock Range] -> Node -> Anf.Let
anfFromTree gr blocks = dfs where
  dfs n =
    let
      label = fromJust (lab gr n)
      block = findBlock blocks label
      children = suc gr n
      childLets = map dfs children
      nodeLet = translateBlock blocks block childLets
    in nodeLet

anfArgs :: [Ast.ArgumentDef Range] -> [Anf.ArgumentDef]
anfArgs = map anfArg

anfArg :: Ast.ArgumentDef Range -> Anf.ArgumentDef
anfArg (Ast.ArgumentDef _ _ (Just name)) = Anf.ArgumentDef (nameToString name)
anfArg (Ast.ArgumentDef _ _ Nothing) = Anf.ArgumentDef "noname"

translateBlock :: [Ast.BasicBlock Range] -> Ast.BasicBlock Range -> [Anf.Let] -> Anf.Let
translateBlock blocks (Ast.BasicBlock _ label phis stmts flow) children = Anf.Let name args blockStmts children blockFlow
  where
    name = nameToString label
    args = argsFromPhis phis
    blockStmts = anfStmts stmts
    blockFlow = flowFromBlock blocks name (fromJust flow)

argsFromPhis :: [Ast.PhiDec Range] -> [Anf.ArgumentDef]
argsFromPhis = map argFromPhi

argFromPhi :: Ast.PhiDec Range -> Anf.ArgumentDef
argFromPhi (Ast.PhiDec _ name _) = Anf.ArgumentDef (nameToString name)

flowFromBlock :: [Ast.BasicBlock Range] -> String -> Ast.Flow Range -> Anf.Flow
flowFromBlock _ _ (Ast.FlowReturn ret) = Anf.FlowCall (anfReturn ret)
flowFromBlock blocks currentLabel (Ast.FlowBranch branch) = anfBranch blocks currentLabel branch

anfBranch :: [Ast.BasicBlock Range] -> String -> Ast.Br Range -> Anf.Flow
anfBranch blocks currentLabel (Ast.Br _ [goto1]) = Anf.FlowCall call
  where
    gotoLabel = nameToString goto1
    block = findBlock blocks gotoLabel
    callArgs = callArgsFromBlockPhis block currentLabel
    call = Anf.Call (Anf.Name gotoLabel) callArgs
anfBranch blocks currentLabel (Ast.Br _ (cond:goto1:goto2:_)) = Anf.FlowCond $ Anf.IfThenElse condConst callIf callElse
  where
    condConst = constFromName cond

    gotoLabelIf = nameToString goto1
    blockIf = findBlock blocks gotoLabelIf
    callArgsIf = callArgsFromBlockPhis blockIf currentLabel
    callIf = Anf.Call (Anf.Name gotoLabelIf) callArgsIf

    gotoLabelElse = nameToString goto2
    blockElse = findBlock blocks gotoLabelElse
    callArgsElse = callArgsFromBlockPhis blockElse currentLabel
    callElse = Anf.Call (Anf.Name gotoLabelElse) callArgsElse
anfBranch _ _ _ = undefined

constFromName :: Ast.Name Range -> Anf.Value
constFromName = Anf.Name . nameToString

callArgsFromBlockPhis :: Ast.BasicBlock Range -> String -> [Anf.Value]
callArgsFromBlockPhis (Ast.BasicBlock _ _ phis _ _) label = map (callArgFromPhi label) phis

callArgFromPhi :: String -> Ast.PhiDec Range -> Anf.Value
callArgFromPhi currentLabel (Ast.PhiDec _ _ (Ast.Phi _ _ values)) = getValueForCurrentLabelFromValues values currentLabel

getValueForCurrentLabelFromValues :: [(Ast.Value Range, Ast.Name Range)] -> String -> Anf.Value
getValueForCurrentLabelFromValues values currentLabel =
  case filter (\(_, name) -> nameToString name == currentLabel) values of
    [(Ast.ValueInt (Ast.IntegerValue _ value), _)] -> Anf.Const value
    [(Ast.ValueName name, _)] -> Anf.Name (nameToString name)
    _ -> error $ "Phi value not found for label " ++ currentLabel

anfReturn :: Ast.Return Range -> Anf.Call
anfReturn (Ast.Return _ _ (Just valueReturned)) = Anf.Call (anfValue valueReturned) []
anfReturn (Ast.Return _ _ Nothing) = Anf.Call (Anf.Const 0) []

anfStmts :: [Ast.Stmt Range] -> [Anf.Expr]
anfStmts = map anfExpr

anfExpr :: Ast.Stmt Range -> Anf.Expr
anfExpr (Ast.SDec stmt) = Anf.ExpDecl (anfDec stmt)
anfExpr (Ast.SCall stmt) = Anf.ExpCall (anfCall stmt)

anfDec :: Ast.Dec Range -> Anf.Decl
anfDec (Ast.DecCall _ name call) = Anf.DeclCall (nameToString name) (anfCall call)
anfDec (Ast.DecBinOp _ name binop) = Anf.DeclBinOp (nameToString name) (anfBinOp binop)
anfDec (Ast.DecConvOp _ name convop) = Anf.DeclConvOp (nameToString name) (anfConvOp convop)
anfDec (Ast.DecIcmp _ name icmp) = Anf.DeclIcmp (nameToString name) (anfIcmp icmp)
anfDec (Ast.DecSelect _ name select) = Anf.DeclSelect (nameToString name) (anfSelect select)

anfConvOp :: Ast.ConvOpCall Range -> Anf.ConvOp
anfConvOp (Ast.ConvOpCall _ _ _ value _) = Anf.ConvOp (anfValue value)

anfSelect :: Ast.Select Range -> Anf.Select
anfSelect (Ast.Select _ _ condValue value1 value2) = Anf.Select (anfValue condValue) (anfValue value1) (anfValue value2)

anfIcmp :: Ast.Icmp Range -> Anf.Icmp
anfIcmp (Ast.Icmp _ (Ast.Cmp _ cmp) _ value1 value2) = Anf.Icmp cmp (anfValue value1) (anfValue value2)

anfCall :: Ast.Call Range -> Anf.Call
anfCall (Ast.Call _ _ name args) = Anf.Call (Anf.Name (nameToString name)) (anfCallArgs args)

anfBinOp :: Ast.BinOpCall Range -> Anf.BinOp
anfBinOp (Ast.BinOpCall _ (Ast.BinOp _ binop) _ value1 value2) = Anf.BinOp binop (anfValue value1) (anfValue value2)

anfCallArgs :: [Ast.CallArgument Range] -> [Anf.Value]
anfCallArgs = map anfCallArg

anfCallArg :: Ast.CallArgument Range -> Anf.Value
anfCallArg (Ast.CallArgument _ _ value) = anfValue value

anfValue :: Ast.Value Range -> Anf.Value
anfValue (Ast.ValueInt (Ast.IntegerValue _ int)) = Anf.Const int
anfValue (Ast.ValueName name) = Anf.Name (nameToString name)
