{-# LANGUAGE GADTs #-}

module PrintAnf (printProgram) where

import Data.List (intercalate)

import Anf
import TranslateAux

import Text.Printf (printf)

header :: String
header = "import Data.Bits\n\n"

printProgram :: Program -> String
printProgram (Program functions) = header ++ intercalate "\n\n" (map printFunction functions)

printFunction :: Function -> String
printFunction (Function name args lets) =
  let
    functionArgs = unrollArguments args
    firstBlockLabel = (\(Lambda letName _ _ _ _) -> letName) lets
  in functionString name functionArgs (printLet lets 2) firstBlockLabel

unrollArguments :: [ArgumentDef] -> String
unrollArguments ((ArgumentDef name):x) = name ++ " " ++ unrollArguments x
unrollArguments [] = ""

printLet :: Lambda -> Int -> String
printLet (Lambda name args exprs lets flow) l =
  let
    argsStr = unrollArguments args
    exprsStr = concatMap (`printExpr` (l + 2)) exprs
    letsStr = concatMap (`printLet` (l + 2)) lets
    flowStr = printFlow flow (l + 1)
  in blockString l name argsStr exprsStr letsStr flowStr

printExpr :: Expr -> Int -> String
printExpr (ExpDecl decl) l = indent l $ printDecl decl
-- printExpr (ExpCall call) l = indent l $ printCall call

printCall :: Call -> String
printCall (Call (Name fname) values) = fname ++ " " ++ unwords (map printValue values)
printCall (Call (Const value) _) = show value
printCall (Call Unit _) = undefined

printDecl :: Decl -> String
printDecl (DeclBinOp name binop) = declString name (printBinOp binop)
printDecl (DeclCall name call) = declString name (printCall call)
printDecl (DeclIcmp name icmp) = declString name (printIcmp icmp)
printDecl (DeclSelect name select) = declString name (printSelect select)
printDecl (DeclConvOp name convop) = declString name (printConvOp convop)

printIcmp :: Icmp -> String
printIcmp (Icmp cmp a b) = printf "if %s %s %s then 1 else 0" (printValue a) (translateCmpType cmp) (printValue b)

printSelect :: Select -> String
printSelect (Select a b c) = printf "if %s == 1 then %s else %s" (printValue a) (printValue b) (printValue c)

printConvOp :: ConvOp -> String
printConvOp (ConvOp value) = printValue value

printBinOp :: BinOp -> String
printBinOp (BinOp op left right) = printValue left ++ translateOperator op ++ printValue right

printValue :: Value -> String
printValue (Const c) = if c < 0 then "(" ++ show c ++ ")" else show c
printValue (Name n) = n
printValue Unit = "()"

printFlow :: Flow -> Int -> String
printFlow (FlowCall call) l = indent l "in " ++ printCall call ++ "\n"
printFlow (FlowCond cond) l = printCond cond l

printCond :: IfThenElse -> Int -> String
printCond (IfThenElse cond thenCall elseCall) l = condString l (printValue cond) (printCall thenCall) (printCall elseCall)

functionString :: String -> String -> String -> String -> String
functionString = printf "%s %s=\n  let\n%s  in %s ()\n"

blockString :: Int -> String -> String -> String -> String -> String -> String
blockString level = printf (indentEach level ["%s %s=\n", "  let\n%s%s%s"])

condString :: Int -> String -> String -> String -> String
condString l = printf (indentEach l ["in if %s == 1\n", "  then %s\n", "  else %s\n"])

declString :: String -> String -> String
declString = printf "%s = %s\n"
