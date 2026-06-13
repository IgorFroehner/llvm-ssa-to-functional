{-# LANGUAGE GADTs #-}

module PrintAnf (printProgram) where

import Data.List (intercalate)

import Anf
import TranslateAux

import Text.Printf (printf)

header :: String
header = "import Data.Bits\nimport Data.Int\nimport Data.Word\n\n"

printProgram :: Program -> String
printProgram (Program functions) = header ++ intercalate "\n\n" (map printFunction functions)

printFunction :: Function -> String
printFunction (Function name args argTypes retType lambda call) =
  let
    functionArgs = unrollArguments args
    firstBlockLabel = printCall call
    signature = signatureString name (intercalate " -> " (argTypes ++ [retType]))
  in signature ++ functionString name functionArgs (printLet lambda 2) firstBlockLabel

unrollArguments :: [ArgumentDef] -> String
unrollArguments ((ArgumentDef name):x) = name ++ " " ++ unrollArguments x
unrollArguments [] = ""

printLet :: Lambda -> Int -> String
printLet (Lambda name args exprs lets flow) level =
  let
    lambdaArgs = unrollArguments args
    bindings = concatMap (`printExpr` (level + 2)) exprs
    nestedLambdas = concatMap (`printLet` (level + 2)) lets
    tailCall = printTailCall flow (level + 1)
  in blockString level name lambdaArgs bindings nestedLambdas tailCall

printExpr :: Expr -> Int -> String
printExpr (ExpDecl decl) l = indent l $ printDecl decl
-- printExpr (ExpCall call) l = indent l $ printCall call

printCall :: Call -> String
printCall (Call (Name fname) values) = fname ++ " " ++ unwords (map printValue values)
printCall (Call (Const value) _) = show value
printCall (Call Unit _) = "()"

printDecl :: Decl -> String
printDecl (DeclBinOp name ty binop) = declString name (annot (printBinOp binop) ty)
printDecl (DeclCall name ty call) = declString name (annot (printCall call) ty)
printDecl (DeclIcmp name ty icmp) = declString name (annot (printIcmp icmp) ty)
printDecl (DeclSelect name ty select) = declString name (annot (printSelect select) ty)
-- A conv op already names its target type, so it is not wrapped again.
printDecl (DeclConvOp name convop) = declString name (printConvOp convop)

-- | Pin a binding's result type: @(expr) :: Int32@.
annot :: String -> String -> String
annot = printf "(%s) :: %s"

printIcmp :: Icmp -> String
printIcmp (Icmp cmp a b) = printf "if %s %s %s then 1 else 0" (printValue a) (translateCmpType cmp) (printValue b)

printSelect :: Select -> String
printSelect (Select a b c) = printf "if %s /= 0 then %s else %s" (printValue a) (printValue b) (printValue c)

printConvOp :: ConvOp -> String
printConvOp (ConvOp op src tgt value) = case op of
  -- zext zero-extends: round-trip through the unsigned word type first.
  "zext" -> printf "fromIntegral (fromIntegral %s :: %s) :: %s" v (widthToWordType src) tgtTy
  -- trunc (narrow) and sext (signed widen) are both a plain signed conversion.
  _      -> printf "fromIntegral %s :: %s" v tgtTy
  where
    v = printValue value
    tgtTy = widthToHsType tgt

printBinOp :: BinOp -> String
printBinOp (BinOp op left right) = printValue left ++ translateOperator op ++ printValue right

printValue :: Value -> String
printValue (Const c) = if c < 0 then "(" ++ show c ++ ")" else show c
printValue (Name n) = n
printValue Unit = "()"

printTailCall :: Flow -> Int -> String
printTailCall (FlowCall call) l = indent l "in " ++ printCall call ++ "\n"
printTailCall (FlowCond cond) l = printCond cond l

printCond :: IfThenElse -> Int -> String
printCond (IfThenElse cond thenCall elseCall) l = condString l (printValue cond) (printCall thenCall) (printCall elseCall)

signatureString :: String -> String -> String
signatureString = printf "%s :: %s\n"

functionString :: String -> String -> String -> String -> String
functionString = printf "%s %s=\n  let\n%s  in %s ()\n"

blockString :: Int -> String -> String -> String -> String -> String -> String
blockString level = printf (indentEach level ["%s %s=\n", "  let\n%s%s%s"])

condString :: Int -> String -> String -> String -> String
condString l = printf (indentEach l ["in if %s /= 0\n", "  then %s\n", "  else %s\n"])

declString :: String -> String -> String
declString = printf "%s = %s\n"
