module PrintAnf (printProgram, haskellBackend) where

import Data.List (intercalate)

import Anf
import Backend (Backend(..))
import TranslateAux
import TypeSystem (Ty(..), rho, widthOf, isFloating)

import Text.Printf (printf)

-- | The Haskell source backend. It is /annotation-blind/: 'printProgram' is
-- parameterised over the annotation type and never inspects it, so by naturality
-- (plan §2.4, T1) it renders an annotated tree exactly as it renders the bare
-- one. Hence the same printer serves @'Anf.Program' 'Effect'@ here and the
-- @'Anf.Program' ()@ produced straight out of "Translate" in tests.
haskellBackend :: Backend
haskellBackend = Backend { backendName = "haskell", render = printProgram }

header :: String
header = "import Data.Bits\n\
         \import Data.Int\n\
         \import Data.Word\n\
         \import GHC.Float (float2Double, double2Float)\n\n"

printProgram :: Program a -> String
printProgram (Program functions) = header ++ intercalate "\n\n" (map printFunction functions)

printFunction :: Function a -> String
printFunction (Function _ name args argTypes retType lambda call) =
  let
    functionArgs = unrollArguments args
    firstBlockLabel = printCall call
    signature = signatureString name (intercalate " -> " (map rho (argTypes ++ [retType])))
  in signature ++ functionString name functionArgs (printLet lambda 2) firstBlockLabel

unrollArguments :: [ArgumentDef a] -> String
unrollArguments ((ArgumentDef _ name):x) = name ++ " " ++ unrollArguments x
unrollArguments [] = ""

printLet :: Lambda a -> Int -> String
printLet (Lambda _ name args exprs lets flow) level =
  let
    lambdaArgs = unrollArguments args
    bindings = concatMap (`printExpr` (level + 2)) exprs
    nestedLambdas = concatMap (`printLet` (level + 2)) lets
    tailCall = printTailCall flow (level + 1)
  in blockString level name lambdaArgs bindings nestedLambdas tailCall

printExpr :: Expr a -> Int -> String
printExpr (ExpDecl decl) l = indent l $ printDecl decl
-- printExpr (ExpCall call) l = indent l $ printCall call

printCall :: Call a -> String
printCall (Call _ (Name _ fname) values) = fname ++ " " ++ unwords (map printValue values)
printCall (Call _ (Const _ value) _) = show value
printCall (Call _ c@(FConst {}) _) = printValue c
printCall (Call _ c@(BConst _ _) _) = printValue c
printCall (Call _ (Unit _) _) = "()"

printDecl :: Decl a -> String
printDecl (DeclBinOp _ name ty binop) = declString name (annot (printBinOp ty binop) ty)
printDecl (DeclCall _ name ty call) = declString name (annot (printCall call) ty)
printDecl (DeclIcmp _ name ty icmp) = declString name (annot (printIcmp icmp) ty)
printDecl (DeclSelect _ name ty select) = declString name (annot (printSelect select) ty)
-- A conv op already names its target type, so it is not wrapped again.
printDecl (DeclConvOp _ name convop) = declString name (printConvOp convop)
-- freeze is the identity: emit a typed alias @name = (value) :: IntN@.
printDecl (DeclFreeze _ name ty value) = declString name (annot (printValue value) ty)

-- | Pin a binding's result type: @(expr) :: Int32@.
annot :: String -> Ty -> String
annot expr ty = printf "(%s) :: %s" expr (rho ty)

-- An @icmp@\/@fcmp@ result is a 'Bool' (its 'DeclIcmp' 'Ty' is 'TyBool', so the
-- enclosing 'annot' pins @:: Bool@); emit the bare comparison and let the branch
-- / select / return that consumes it use it directly.
printIcmp :: Icmp a -> String
printIcmp (Icmp _ cmp ty a b) = cmpExpr ty cmp (printValue a) (printValue b)

-- | The Haskell boolean expression for a comparison predicate.
--
-- Integer @icmp@ predicates and floating @fcmp@ /ordered/ predicates map to a
-- plain operator: Haskell's @<@\/@>@\/@==@ already yield 'False' on NaN, which
-- is exactly LLVM's ordered semantics. The float /unordered/ predicates
-- (@u{gt,ge,lt,le}@, @ueq@), which are 'True' whenever an operand is NaN, and
-- the ordered @one@ (which requires both operands non-NaN) need explicit
-- 'isNaN' guards to stay faithful. (@une@ already matches Haskell @/=@: both
-- are 'True' on NaN.)
cmpExpr :: Ty -> String -> String -> String -> String
cmpExpr ty cmp a b
  | isFloating ty = case cmp of
      "ugt" -> unordered ">"
      "uge" -> unordered ">="
      "ult" -> unordered "<"
      "ule" -> unordered "<="
      "ueq" -> unordered "=="
      "one" -> printf "%s == %s && %s == %s && %s /= %s" a a b b a b
      _     -> plain
  | otherwise = plain
  where
    plain = printf "%s %s %s" a (translateCmpType cmp) b
    -- unordered: true if either operand is NaN, or the ordered relation holds.
    unordered :: String -> String
    unordered op = printf "isNaN %s || isNaN %s || %s %s %s" a b a op b

-- The select condition is an i1 ('Bool'), so it drives @if@ directly.
printSelect :: Select a -> String
printSelect (Select _ a b c) = printf "if %s then %s else %s" (printValue a) (printValue b) (printValue c)

printConvOp :: ConvOp a -> String
-- An i1 ('Bool') source: clang's @zext i1@\/@sext i1@ reintroduce the integer
-- 0/1 (or 0/-1 for sext) that a comparison result is widened back into. This is
-- the Bool->int boundary coercion; widths play no role, so it precedes the
-- width-driven cases below.
printConvOp (ConvOp _ op TyBool tgt value) =
  printf "(if %s then %s else 0) :: %s" (printValue value) trueVal (rho tgt)
  where trueVal = if op == "sext" then "(-1)" else "1" :: String
printConvOp (ConvOp _ op src tgt value) = case op of
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

-- The 'Ty' selects the boolean reading of @and@\/@or@\/@xor@ on i1 operands:
-- on 'TyBool' they are the logical connectives (@&&@\/@||@\/@\/=@), not the
-- 'Data.Bits' word operators their integer counterparts use.
printBinOp :: Ty -> BinOp a -> String
printBinOp TyBool (BinOp _ op left right)
  | op `elem` ["and", "or", "xor"] = printValue left ++ boolOperator op ++ printValue right
printBinOp _ (BinOp _ op left right) = printValue left ++ translateOperator op ++ rhs
  where
    -- Haskell's shift functions take the amount as `Int`, but LLVM types both
    -- shift operands at the same iN, so the (sized) amount needs coercing.
    rhs | op `elem` ["shl", "lshr", "ashr"] = "(fromIntegral " ++ printValue right ++ ")"
        | otherwise                         = printValue right

-- | The Haskell logical connective for an @i1@ @and@\/@or@\/@xor@. @xor@ on
-- 'Bool' is inequality (@\/=@), which is also how clang spells logical negation
-- (@xor i1 %c, true@ == @c \/= True@ == @not c@).
boolOperator :: String -> String
boolOperator "and" = " && "
boolOperator "or"  = " || "
boolOperator "xor" = " /= "
boolOperator op    = error ("boolOperator: not a boolean op: " ++ op)

printValue :: Value a -> String
printValue (Const _ c) = if c < 0 then "(" ++ show c ++ ")" else show c
-- A floating literal is always parenthesised and explicitly typed, so it is
-- unambiguous as an operand/argument and pins Float vs Double (no defaulting).
printValue (FConst _ txt ty) = printf "(%s :: %s)" txt (rho ty)
printValue (BConst _ b) = if b then "True" else "False"
printValue (Name _ n) = n
printValue (Unit _) = "()"

printTailCall :: Flow a -> Int -> String
printTailCall (FlowCall call) l = indent l "in " ++ printCall call ++ "\n"
printTailCall (FlowCond cond) l = printCond cond l

printCond :: IfThenElse a -> Int -> String
printCond (IfThenElse _ cond thenCall elseCall) l = condString l (printValue cond) (printCall thenCall) (printCall elseCall)

signatureString :: String -> String -> String
signatureString = printf "%s :: %s\n"

functionString :: String -> String -> String -> String -> String
functionString = printf "%s %s=\n  let\n%s  in %s ()\n"

blockString :: Int -> String -> String -> String -> String -> String -> String
blockString level = printf (indentEach level ["%s %s=\n", "  let\n%s%s%s"])

-- A conditional @br@ tests an i1 ('Bool') condition, so it drives @if@ directly.
condString :: Int -> String -> String -> String -> String
condString l = printf (indentEach l ["in if %s\n", "  then %s\n", "  else %s\n"])

declString :: String -> String -> String
declString = printf "%s = %s\n"
