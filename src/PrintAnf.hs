{-# LANGUAGE GADTs #-}

module PrintAnf (printProgram) where

import Data.List (intercalate)
import Text.Printf (printf)

import Anf
import TranslateAux

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.ByteString.Lazy.Char8 (ByteString)

header :: ByteString
header = "import Data.Bits\n\n"

printProgram :: Program -> ByteString
printProgram (Program functions) = LBS.append header (LBS.intercalate "\n\n" (map printFunction functions))

printFunction :: Function -> ByteString
printFunction (Function name args lets) =
  let
    functionArgs = unrollArguments args
    firstBlockLabel = (\(Lambda letName _ _ _ _) -> letName) lets
  in functionString name functionArgs (printLet lets 2) firstBlockLabel

unrollArguments :: [ArgumentDef] -> ByteString
unrollArguments ((ArgumentDef name):x) = name <> " " <> unrollArguments x
unrollArguments [] = ""

printLet :: Lambda -> Int -> ByteString
printLet (Lambda name args exprs lets flow) level =
  let
    argsStr = unrollArguments args
    exprsStr = concatMap (`printExpr` (level + 2)) exprs
    letsStr = concatMap (`printLet` (level + 2)) lets
    flowStr = printFlow flow (level + 1)
  in blockString level name argsStr exprsStr letsStr flowStr

printExpr :: Expr -> Int -> ByteString
printExpr (ExpDecl decl) level = indent level $ printDecl decl
-- printExpr (ExpCall call) l = indent l $ printCall call

printCall :: Call -> String
printCall (Call (Name fname) values) = fname ++ " " ++ unwords (map printValue values)
printCall (Call (Const value) _) = show value
printCall (Call Unit _) = undefined

printDecl :: Decl -> ByteString
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
printFlow (FlowCall call) level = {-indent level "in " ++-} printCall call
printFlow (FlowCond cond) level = printCond cond level

printCond :: IfThenElse -> Int -> String
printCond (IfThenElse cond thenCall elseCall) level = condString level (printValue cond) (printCall thenCall) (printCall elseCall)

functionString :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString
functionString name args lets firstBlockLabel = LBS.pack $ printf "%s %s=\n  let\n%s  in %s ()\n" name args lets firstBlockLabel

-- functionString :: String -> String -> String -> String -> String
-- functionString = printf "%s %s=\n  let\n%s  in %s ()\n"

blockString :: Int -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString
blockString level label args decs lambas jump = LBS.pack $ printf (indentEach level ["%s %s=\n", "  let\n%s%s", "  in %s\n"]) label args decs lambas jump

condString :: Int -> String -> String -> String -> String
condString level = printf ("if %s == 1\n" ++ indentEach level ["  then %s\n", "  else %s"])

declString :: String -> String -> String
declString = printf "%s = %s\n"
