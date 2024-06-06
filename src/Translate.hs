
module Translate (translate) where

import Lexer

import qualified Ast
import qualified Anf

import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph (lab, suc, Node)
import Data.Maybe (fromJust)

import TranslateAux
import AstHelpers

import Dominance (buildGraph, dominance)

translate :: Ast.Program Range -> Anf.Program
translate (Ast.Program fs) = Anf.Program $ map translateFunction fs

translateFunction :: Ast.Function Range -> Anf.Function
translateFunction function = buildAnfFromFunction function dom
  where
    g = buildGraph function
    dom = dominance g

buildAnfFromFunction :: Ast.Function Range -> Gr String () -> Anf.Function
buildAnfFromFunction (Ast.FunctionDef _ _ name args blocks) dom = Anf.Function (nameToString name) (anfArgs args) lambda
  where
    lambda = anfFromTree blocks dom 0
-- buildAnfFromFunction _ _ = undefined

anfFromTree :: [Ast.BasicBlock Range] -> Gr String () -> Node -> Anf.Lambda
anfFromTree blocks gr node =
  let
    label = fromJust (lab gr node)
    block = findBlock blocks label
    children = suc gr node
    nestedLambdas = map (anfFromTree blocks gr) children

    translateBlock :: Ast.BasicBlock Range -> [Anf.Lambda] -> Anf.Lambda
    translateBlock (Ast.BasicBlock _ _ phis stmts flow) nested =
      let
        args = argsFromPhis phis
        blockBindings = anfBindings stmts
        tailCall = tailCallFromBlock blocks label flow
      in Anf.Lambda label args blockBindings nested tailCall

  in translateBlock block nestedLambdas

anfArgs :: [Ast.ArgumentDef Range] -> [Anf.ArgumentDef]
anfArgs [] = [Anf.ArgumentDef "()"]
anfArgs args =
  let
    anfArg :: Ast.ArgumentDef Range -> Anf.ArgumentDef
    anfArg (Ast.ArgumentDef _ _ (Just name)) = Anf.ArgumentDef (nameToString name)
    anfArg (Ast.ArgumentDef _ _ Nothing) = Anf.ArgumentDef "noname"
  in map anfArg args

argsFromPhis :: [Ast.PhiDec Range] -> [Anf.ArgumentDef]
argsFromPhis [] = [Anf.ArgumentDef "()"]
argsFromPhis phis =
  let
    argFromPhi :: Ast.PhiDec Range -> Anf.ArgumentDef
    argFromPhi (Ast.PhiDec _ name _) = Anf.ArgumentDef (nameToString name)
  in map argFromPhi phis

tailCallFromBlock :: [Ast.BasicBlock Range] -> String -> Ast.Flow Range -> Anf.Flow
tailCallFromBlock _ _ (Ast.FlowReturn ret) = Anf.FlowCall (anfReturn ret)
tailCallFromBlock blocks currentLabel (Ast.FlowBranch branch) = anfBranch blocks currentLabel branch

anfBranch :: [Ast.BasicBlock Range] -> String -> Ast.Br Range -> Anf.Flow
anfBranch blocks currentLabel (Ast.Br _ [goto]) =
  let
    call = callFromGoto blocks currentLabel goto
  in Anf.FlowCall call
anfBranch blocks currentLabel (Ast.Br _ (cond:gotoIf:gotoElse:_)) = 
  let
    condValue = valueFromName cond

    callIf = callFromGoto blocks currentLabel gotoIf
    callElse = callFromGoto blocks currentLabel gotoElse
  in Anf.FlowCond $ Anf.IfThenElse condValue callIf callElse
anfBranch _ _ _ = undefined

callFromGoto :: [Ast.BasicBlock Range] -> String -> Ast.Name Range -> Anf.Call
callFromGoto blocks currentLabel gotoLabel =
  let
    gotoName = nameToString gotoLabel
    block = findBlock blocks gotoName
    anfName = Anf.Name gotoName
    anfArgsCall = callArgsFromBlockPhis block currentLabel
  in Anf.Call anfName anfArgsCall

callArgsFromBlockPhis :: Ast.BasicBlock Range -> String -> [Anf.Value]
callArgsFromBlockPhis (Ast.BasicBlock _ _ [] _ _) _ = [Anf.Unit]
callArgsFromBlockPhis (Ast.BasicBlock _ _ phis _ _) label = map (callArgFromPhi label) phis

callArgFromPhi :: String -> Ast.PhiDec Range -> Anf.Value
callArgFromPhi currentLabel (Ast.PhiDec _ _ (Ast.Phi _ _ values)) = getValueForCurrentLabel values currentLabel

getValueForCurrentLabel :: [(Ast.Value Range, Ast.Name Range)] -> String -> Anf.Value
getValueForCurrentLabel values currentLabel =
  case filter (\(_, name) -> nameToString name == currentLabel) values of
    [(Ast.ValueInt (Ast.IntegerValue _ value), _)] -> Anf.Const value
    [(Ast.ValueName name, _)] -> Anf.Name (nameToString name)
    _ -> error $ "Phi value not found for label " ++ currentLabel

anfReturn :: Ast.Return Range -> Anf.Call
anfReturn (Ast.Return _ _ (Just valueReturned)) = Anf.Call (anfValue valueReturned) []
anfReturn (Ast.Return _ _ Nothing) = Anf.Call (Anf.Const 0) []

anfBindings :: [Ast.Stmt Range] -> [Anf.Expr]
anfBindings = map anfExpr

anfExpr :: Ast.Stmt Range -> Anf.Expr
anfExpr (Ast.SDec stmt) = Anf.ExpDecl (anfDec stmt)
-- anfExpr (Ast.SCall stmt) = Anf.ExpCall (anfCall stmt)

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

valueFromName :: Ast.Name Range -> Anf.Value
valueFromName = Anf.Name . nameToString
