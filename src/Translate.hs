
module Translate (translate) where

import Lexer

import qualified Ast
import TranslateAux
import Text.Printf

translate :: [Ast.Function Range] -> String
translate = concatMap translateFunction

translateFunction :: Ast.Function Range -> String
translateFunction (Ast.FunctionDef _ _ name args blocks) = functionString (uname name) (placeFunctionArgs args) (translateBlocks blocks)
translateFunction _ = ""

placeFunctionArgs :: [Ast.ArgumentDef Range] -> String
placeFunctionArgs [a] = placeFunctionArg a
placeFunctionArgs (a:x) = placeFunctionArg a ++ " " ++ placeFunctionArgs x
placeFunctionArgs [] = ""

placeFunctionArg :: Ast.ArgumentDef Range -> String
placeFunctionArg (Ast.ArgumentDef _ _ (Just name)) = uname name
placeFunctionArg (Ast.ArgumentDef _ _ Nothing) = "-"

translateBlocks :: [Ast.BasicBlock Range] -> String
translateBlocks = concatMap translateBlock

-- BasicBlock a (Maybe (Name a)) [PhiDec a] [Stmt a] (Maybe (Flow a))
translateBlock :: Ast.BasicBlock Range -> String
translateBlock (Ast.BasicBlock _ Nothing phis stmts (Just flow)) = blockString "f" (getArgsFromPhis phis) (translateStmts stmts) (translateFlow flow) "f"
translateBlock (Ast.BasicBlock _ (Just label) phis stmts (Just flow)) = blockString (uname label) (getArgsFromPhis phis) (translateStmts stmts) (translateFlow flow) (uname label)
translateBlock (Ast.BasicBlock _ Nothing phis stmts Nothing) = blockString "f" (getArgsFromPhis phis) (translateStmts stmts) "" "f"
translateBlock (Ast.BasicBlock _ (Just label) phis stmts Nothing) = blockString (uname label) (getArgsFromPhis phis) (translateStmts stmts) "" (uname label)

getArgsFromPhis :: [Ast.PhiDec Range] -> String
getArgsFromPhis [phi] = getArgFromPhi phi
getArgsFromPhis (phi:x) = getArgFromPhi phi ++ " " ++ getArgsFromPhis x
getArgsFromPhis [] = ""

getArgFromPhi :: Ast.PhiDec Range -> String
getArgFromPhi (Ast.PhiDec _ name _) = uname name

translateStmts :: [Ast.Stmt Range] -> String
translateStmts = concatMap translateStmt

translateStmt :: Ast.Stmt Range -> String
translateStmt (Ast.SDec stmt) = translateDec stmt
translateStmt (Ast.SCall stmt) = translateCall stmt

translateDec :: Ast.Dec Range -> String
translateDec (Ast.DecCall _ name call) = "        " ++ uname name ++ " = " ++ translateCallDec call ++ "\n"
translateDec (Ast.DecIcmp _ name icmp) = "        " ++ uname name ++ " = " ++ translateICMP icmp ++ "\n"
translateDec (Ast.DecBinOp _ name binop) = "        " ++ uname name ++ " = " ++ translateBinOp binop ++ "\n"
-- translateDec (Ast.DecConvOp _ name convop) = "  Dec " ++ uname name ++ " = " ++ unconvop convop ++ "\n"
-- translateDec (Ast.DecSelect _ name select) = "  Dec " ++ uname name ++ " = " ++ unselect select ++ "\n"
translateDec _ = "Unknown"

translateCallDec :: Ast.Call Range -> String
translateCallDec (Ast.Call _ _ (Ast.GName _ name) args) = unpack name ++ " " ++ translateArgs args
translateCallDec _ = "Unkonw"

translateCall :: Ast.Call Range -> String
translateCall (Ast.Call _ _ (Ast.GName _ name) args) = "        " ++ unpack name ++ " " ++ translateArgs args ++ "\n"
translateCall _ = "Unkonw"

translateArgs :: [Ast.CallArgument Range] -> String
translateArgs ((Ast.CallArgument _ _ value):x) = unvalue value ++ " " ++ translateArgs x
translateArgs [] = ""

translateFlow :: Ast.Flow Range -> String
translateFlow (Ast.FlowBranch _) = "      calmo \n"
translateFlow (Ast.FlowReturn ret) = translateReturn ret

translateReturn :: Ast.Return Range -> String
translateReturn (Ast.Return _ _ (Just value)) = "      in " ++ unvalue value ++ "\n"
translateReturn (Ast.Return _ _ Nothing) = ""

functionString :: String -> String -> String -> String
functionString = printf "%s %s = let in\n %s"

blockString :: String -> String -> String -> String -> String -> String
blockString = printf "  let \n    %s %s =\n      let\n%s %s    in %s\n"
