{-# LANGUAGE GADTs #-}

module PrintAnf (printProgram) where

import Data.List (intercalate)

import Anf
import TranslateAux
import TypeSystem (Ty, rho, widthOf)

import Text.Printf (printf)

header :: String
header = "import Data.Bits\n\
         \import Data.Int\n\
         \import Data.Word\n\
         \import Data.Fixed (mod')\n\
         \import GHC.Float (float2Double, double2Float)\n\n"

printProgram :: Program -> String
printProgram (Program functions) = header ++ intercalate "\n\n" (map printFunction functions)

printFunction :: Function -> String
printFunction (Function name args argTypes retType lambda call) =
  let
    functionArgs = unrollArguments args
    firstBlockLabel = printCall call
    signature = signatureString name (intercalate " -> " (map rho (argTypes ++ [retType])))
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
printCall (Call c@(FConst _ _) _) = printValue c
printCall (Call Unit _) = "()"

printDecl :: Decl -> String
printDecl (DeclBinOp name ty binop) = declString name (annot (printBinOp binop) ty)
printDecl (DeclCall name ty call) = declString name (annot (printCall call) ty)
printDecl (DeclIcmp name ty icmp) = declString name (annot (printIcmp icmp) ty)
printDecl (DeclSelect name ty select) = declString name (annot (printSelect select) ty)
-- A conv op already names its target type, so it is not wrapped again.
printDecl (DeclConvOp name convop) = declString name (printConvOp convop)
-- freeze is the identity: emit a typed alias @name = (value) :: IntN@.
printDecl (DeclFreeze name ty value) = declString name (annot (printValue value) ty)

-- | Pin a binding's result type: @(expr) :: Int32@.
annot :: String -> Ty -> String
annot expr ty = printf "(%s) :: %s" expr (rho ty)

printIcmp :: Icmp -> String
printIcmp (Icmp cmp a b) = printf "if %s %s %s then 1 else 0" (printValue a) (translateCmpType cmp) (printValue b)

printSelect :: Select -> String
printSelect (Select a b c) = printf "if %s /= 0 then %s else %s" (printValue a) (printValue b) (printValue c)

printConvOp :: ConvOp -> String
printConvOp (ConvOp op src tgt value) = case op of
  -- zext / uitofp zero-extend the source: round-trip through its unsigned word
  -- type first, so the sign bit is not propagated.
  "zext"    -> printf "fromIntegral (fromIntegral %s :: %s) :: %s" v (wordOf src) tgtTy
  "uitofp"  -> printf "fromIntegral (fromIntegral %s :: %s) :: %s" v (wordOf src) tgtTy
  -- float -> int is truncation toward zero (LLVM semantics), i.e. `truncate`.
  "fptosi"  -> printf "truncate %s :: %s" v tgtTy
  "fptoui"  -> printf "fromIntegral (truncate %s :: %s) :: %s" v (wordOf tgt) tgtTy
  -- float <-> double use the bit-exact GHC.Float primitives.
  "fpext"   -> printf "float2Double %s" v
  "fptrunc" -> printf "double2Float %s" v
  -- trunc (narrow), sext (signed widen) and sitofp are all a plain signed
  -- `fromIntegral` into the target type.
  _         -> printf "fromIntegral %s :: %s" v tgtTy
  where
    v = printValue value
    tgtTy = rho tgt
    wordOf = widthToWordType . widthOf

printBinOp :: BinOp -> String
printBinOp (BinOp op left right) = printValue left ++ translateOperator op ++ rhs
  where
    -- Haskell's shift functions take the amount as `Int`, but LLVM types both
    -- shift operands at the same iN, so the (sized) amount needs coercing.
    rhs | op `elem` ["shl", "lshr", "ashr"] = "(fromIntegral " ++ printValue right ++ ")"
        | otherwise                         = printValue right

printValue :: Value -> String
printValue (Const c) = if c < 0 then "(" ++ show c ++ ")" else show c
-- A floating literal is always parenthesised and explicitly typed, so it is
-- unambiguous as an operand/argument and pins Float vs Double (no defaulting).
printValue (FConst txt ty) = printf "(%s :: %s)" txt (rho ty)
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
