
module Translate (translate) where

import Lexer

import qualified Ast
import qualified Anf

import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph (lab, suc, Node)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)

import TranslateAux
import TypeSystem (Ty(..), elaborate)
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
buildAnfFromFunction (Ast.FunctionDef _ retTy name args blocks) dom =
  Anf.Function (nameToString name) (anfArgs args) (argTypes args) (typeStr retTy) lambda callFirstBlock
  where
    lambda = anfFromTree blocks dom 0
    firstBlockLabel = firstBlockName blocks
    callFirstBlock = Anf.Call (Anf.Name firstBlockLabel) []
-- buildAnfFromFunction _ _ = undefined

-- | Elaborate an LLVM type annotation into the internal 'Ty'.
typeStr :: Ast.Type Range -> Ty
typeStr (Ast.Type _ t) = elaborate t

-- | Argument types for the function signature. A nullary LLVM function takes a
-- single @()@ (mirroring 'anfArgs').
argTypes :: [Ast.ArgumentDef Range] -> [Ty]
argTypes [] = [TyUnit]
argTypes as = map (\(Ast.ArgumentDef _ t _) -> typeStr t) as

firstBlockName :: [Ast.BasicBlock Range] -> String
firstBlockName = getLabel . head

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
callArgFromPhi currentLabel (Ast.PhiDec _ _ (Ast.Phi _ ty values)) = getValueForCurrentLabel (typeStr ty) values currentLabel

-- | Resolve a φ-incoming value for the source block, typed by the φ-node's
-- declared type (so a floating literal is tagged 'Anf.FConst' correctly).
getValueForCurrentLabel :: Ty -> [(Ast.Value Range, Ast.Name Range)] -> String -> Anf.Value
getValueForCurrentLabel ty values currentLabel =
  case filter (\(_, name) -> nameToString name == currentLabel) values of
    [(value, _)] -> anfValue ty value
    _ -> error $ "Phi value not found for label " ++ currentLabel

anfReturn :: Ast.Return Range -> Anf.Call
anfReturn (Ast.Return _ ty (Just valueReturned)) = Anf.Call (anfValue (typeStr ty) valueReturned) []
anfReturn (Ast.Return _ _ Nothing) = Anf.Call Anf.Unit []

anfBindings :: [Ast.Stmt Range] -> [Anf.Expr]
anfBindings = map anfExpr

anfExpr :: Ast.Stmt Range -> Anf.Expr
anfExpr (Ast.SDec stmt) = Anf.ExpDecl (anfDec stmt)
-- anfExpr (Ast.SCall stmt) = Anf.ExpCall (anfCall stmt)

anfDec :: Ast.Dec Range -> Anf.Decl
anfDec (Ast.DecCall _ name call@(Ast.Call _ ty _ _)) = Anf.DeclCall (nameToString name) (typeStr ty) (anfCall call)
anfDec (Ast.DecBinOp _ name binop@(Ast.BinOpCall _ _ ty _ _)) = Anf.DeclBinOp (nameToString name) (typeStr ty) (anfBinOp binop)
anfDec (Ast.DecConvOp _ name convop) = Anf.DeclConvOp (nameToString name) (anfConvOp convop)
-- icmp/fcmp always yield an i1 (= 'TyBool'), regardless of their (operand) type.
anfDec (Ast.DecIcmp _ name icmp) = Anf.DeclIcmp (nameToString name) TyBool (anfIcmp icmp)
anfDec (Ast.DecSelect _ name select@(Ast.Select _ ty _ _ _)) = Anf.DeclSelect (nameToString name) (typeStr ty) (anfSelect (typeStr ty) select)
anfDec (Ast.DecFreeze _ name (Ast.Freeze _ ty value)) = Anf.DeclFreeze (nameToString name) (typeStr ty) (anfValue (typeStr ty) value)

anfConvOp :: Ast.ConvOpCall Range -> Anf.ConvOp
anfConvOp (Ast.ConvOpCall _ (Ast.ConvOp _ op) srcT value tgtT) =
  Anf.ConvOp op (typeStr srcT) (typeStr tgtT) (anfValue (typeStr srcT) value)

-- | The select condition is an i1 (typed 'TyBool'); both arms carry the
-- result type @ty@.
anfSelect :: Ty -> Ast.Select Range -> Anf.Select
anfSelect ty (Ast.Select _ _ condValue value1 value2) =
  Anf.Select (anfValue TyBool condValue) (anfValue ty value1) (anfValue ty value2)

anfIcmp :: Ast.Icmp Range -> Anf.Icmp
anfIcmp (Ast.Icmp _ (Ast.Cmp _ cmp) ty value1 value2) =
  Anf.Icmp cmp (typeStr ty) (anfValue (typeStr ty) value1) (anfValue (typeStr ty) value2)

anfCall :: Ast.Call Range -> Anf.Call
anfCall (Ast.Call _ _ name args) =
  let
    callee = nameToString name
    callArgs = anfCallArgs args
  in case intrinsicRewrite callee callArgs of
    Just call -> call
    Nothing   -> Anf.Call (Anf.Name callee) callArgs

-- | Rewrite the pure integer LLVM intrinsics clang emits at @-O1@ to their
-- Prelude equivalents. Names arrive here already punctuation-stripped by
-- 'NameNormalizer' (@\@llvm.abs.i32@ -> @llvmabsi32@), so the match is on that
-- normalized prefix. Only @llvm.*@ is rewritten; an ordinary same-module call
-- yields 'Nothing' and falls through unchanged. @llvm.abs@'s second argument is
-- the @i1@ poison immarg, which has no Haskell counterpart and is dropped.
intrinsicRewrite :: String -> [Anf.Value] -> Maybe Anf.Call
intrinsicRewrite callee args
  | "llvmabs"  `isPrefixOf` callee = Just (Anf.Call (Anf.Name "abs") (take 1 args))
  | "llvmsmin" `isPrefixOf` callee = Just (Anf.Call (Anf.Name "min") args)
  | "llvmsmax" `isPrefixOf` callee = Just (Anf.Call (Anf.Name "max") args)
  | otherwise                      = Nothing

anfBinOp :: Ast.BinOpCall Range -> Anf.BinOp
anfBinOp (Ast.BinOpCall _ (Ast.BinOp _ binop) ty value1 value2) =
  Anf.BinOp binop (anfValue (typeStr ty) value1) (anfValue (typeStr ty) value2)

anfCallArgs :: [Ast.CallArgument Range] -> [Anf.Value]
anfCallArgs = map anfCallArg

anfCallArg :: Ast.CallArgument Range -> Anf.Value
anfCallArg (Ast.CallArgument _ ty value) = anfValue (typeStr ty) value

-- | Translate an operand. The contextual 'Ty' (from the enclosing instruction's
-- annotation) is only consulted for floating literals, which must be tagged so
-- the printer emits @Float@\/@Double@ and an explicitly-typed literal; names and
-- integer literals print the same regardless.
anfValue :: Ty -> Ast.Value Range -> Anf.Value
-- An i1 literal (LLVM @true@\/@false@, lexed as 1\/0) is a 'Bool' in context.
anfValue TyBool (Ast.ValueInt (Ast.IntegerValue _ int)) = Anf.BConst (int /= 0)
anfValue _ (Ast.ValueInt (Ast.IntegerValue _ int)) = Anf.Const int
anfValue _ (Ast.ValueName name) = Anf.Name (nameToString name)
anfValue ty (Ast.ValueFloat (Ast.FloatValue _ txt)) = Anf.FConst txt ty

valueFromName :: Ast.Name Range -> Anf.Value
valueFromName = Anf.Name . nameToString
