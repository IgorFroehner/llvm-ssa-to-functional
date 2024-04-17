
module Translate (translate) where

import Lexer

import qualified Ast
import TranslateAux
import NameNormalizer
import Text.Printf

translate :: [Ast.Function Range] -> String
translate = concatMap translateFunction

translateFunction :: Ast.Function Range -> String
translateFunction (Ast.FunctionDef _ _ name args blocks) = functionString (normalizeName name) (placeFunctionArgs args) (translateBlocks blocks)
translateFunction _ = ""

placeFunctionArgs :: [Ast.ArgumentDef Range] -> String
placeFunctionArgs (a:x) = placeFunctionArg a ++ " " ++ placeFunctionArgs x
placeFunctionArgs [] = ""

placeFunctionArg :: Ast.ArgumentDef Range -> String
placeFunctionArg (Ast.ArgumentDef _ _ (Just name)) = normalizeName name
placeFunctionArg (Ast.ArgumentDef _ _ Nothing) = "-"

translateBlocks :: [Ast.BasicBlock Range] -> String
translateBlocks blocks = translateBlock blocks (head blocks)

translateBlock :: [Ast.BasicBlock Range] -> Ast.BasicBlock Range -> String
translateBlock blocks (Ast.BasicBlock _ Nothing phis stmts (Just flow)) = blockString "f" (getArgsFromPhis phis) (translateStmts stmts) (inlineCalledBlocks blocks flow) (translateFlow flow) "f"
translateBlock blocks (Ast.BasicBlock _ (Just label) phis stmts (Just flow)) = blockString (normalizeBlockName label) (getArgsFromPhis phis) (translateStmts stmts) (inlineCalledBlocks blocks flow) (translateFlow flow) (normalizeBlockName label)
translateBlock _ (Ast.BasicBlock _ Nothing phis stmts Nothing) = blockString "f" (getArgsFromPhis phis) (translateStmts stmts) "" "" "f"
translateBlock _ (Ast.BasicBlock _ (Just label) phis stmts Nothing) = blockString (normalizeBlockName label) (getArgsFromPhis phis) (translateStmts stmts) "" "" (normalizeBlockName label)

getArgsFromPhis :: [Ast.PhiDec Range] -> String
getArgsFromPhis (phi:x) = getArgFromPhi phi ++ " " ++ getArgsFromPhis x
getArgsFromPhis [] = ""

getArgFromPhi :: Ast.PhiDec Range -> String
getArgFromPhi (Ast.PhiDec _ name _) = normalizeName name

translateStmts :: [Ast.Stmt Range] -> String
translateStmts = concatMap translateStmt

translateStmt :: Ast.Stmt Range -> String
translateStmt (Ast.SDec stmt) = translateDec stmt
translateStmt (Ast.SCall stmt) = translateCall stmt

translateDec :: Ast.Dec Range -> String
translateDec (Ast.DecCall _ name call) = decString (normalizeName name) (translateCallDec call)
translateDec (Ast.DecIcmp _ name icmp) = decString (normalizeName name) (translateICMP icmp)
translateDec (Ast.DecBinOp _ name binop) = decString (normalizeName name) (translateBinOp binop)
translateDec (Ast.DecConvOp _ name convop) = decString (normalizeName name) (translateConvOp convop)
translateDec (Ast.DecSelect _ name select) = decString (normalizeName name) (translateSelect select)

translateConvOp :: Ast.ConvOpCall Range -> String
translateConvOp (Ast.ConvOpCall _ _ _ value _) = unvalue value

translateCallDec :: Ast.Call Range -> String
translateCallDec (Ast.Call _ _ name args) = normalizeName name ++ " " ++ translateArgs args

translateCall :: Ast.Call Range -> String
translateCall (Ast.Call _ _ name args) = "        " ++ normalizeName name ++ " " ++ translateArgs args ++ "\n"

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
functionString = printf "%s %s= let in\n\
                        \%s\n"

blockString :: String -> String -> String -> String -> String -> String -> String
blockString = printf "  let \n\
                     \    %s %s=\n\
                     \      let\n\
                     \%s%s%s\
                     \    in %s\n"

decString :: String -> String -> String
decString = printf "        %s = %s\n"

returnString :: String -> String
returnString = printf "      in %s\n"

inlinedBlockString :: String -> String -> String -> String -> String
inlinedBlockString = printf "        %s %s= let\n\
                            \        %s\
                            \        %s"

brIfString :: String -> String -> String -> String
brIfString = printf "      in if %s == 1\n\
                    \        then %s\n\
                    \        else %s\n"

translateBranch :: Ast.Br Range -> String
translateBranch (Ast.Br _ [(_, name)]) = "        in " ++ normalizeBlockName name ++ "\n"
translateBranch (Ast.Br _ ((_, cond):(_, name1):(_, name2):_)) = brIfString (normalizeName cond) (normalizeBlockName name1) (normalizeBlockName name2)
translateBranch _ = "Unkown branch type"

inlineCalledBlocks :: [Ast.BasicBlock Range] -> Ast.Flow Range -> String
inlineCalledBlocks blocks (Ast.FlowBranch br) = calledBlocks blocks br
inlineCalledBlocks _ (Ast.FlowReturn _) = ""

calledBlocks :: [Ast.BasicBlock Range] -> Ast.Br Range -> String
calledBlocks blocks (Ast.Br _ [(_, name)]) = translateInlineBlock (findBlock blocks (normalizeBlockName name))
calledBlocks blocks (Ast.Br _ ((_, _):(_, name1):(_, name2):_)) = translateInlineBlock (findBlock blocks (normalizeBlockName name1)) ++ translateInlineBlock (findBlock blocks (normalizeBlockName name2))
calledBlocks _ (Ast.Br _ _) = "Unkown branch type"

translateInlineBlock :: Ast.BasicBlock Range -> String
translateInlineBlock (Ast.BasicBlock _ (Just label) phis stmts (Just flow)) = inlinedBlockString (normalizeBlockName label) (getArgsFromPhis phis) (translateStmts stmts) (translateFlow flow)
translateInlineBlock (Ast.BasicBlock _ (Just label) phis stmts Nothing) = inlinedBlockString (normalizeBlockName label) (getArgsFromPhis phis) (translateStmts stmts) ""
translateInlineBlock (Ast.BasicBlock _ Nothing _ _ _) = "Inlined Blocks must have a label"

findBlock :: [Ast.BasicBlock Range] -> String -> Ast.BasicBlock Range
findBlock (a:x) name = if getLabel a == name then a else findBlock x name
findBlock [] name = error (printf "Block %s not found" name)

getLabel :: Ast.BasicBlock Range -> String
getLabel (Ast.BasicBlock _ (Just label) _ _ _) = normalizeBlockName label
getLabel (Ast.BasicBlock _ Nothing _ _ _) = ""
