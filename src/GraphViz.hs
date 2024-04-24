
module GraphViz (
    -- genAst
    plotControlGraph,
    plotDominanceTree
    ) where

import Lexer
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Ast

import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph (labNodes, labEdges, LNode, LEdge)
import Dominance (dominance)

-- GraphViz the Dominance Tree

findNodeLabel :: Int -> [(Int, String)] -> String
findNodeLabel n nodes = case lookup n nodes of
  Just label -> label
  Nothing -> error $ "Node not found: " ++ show n

plotDominanceTree :: Gr String () -> String
plotDominanceTree g =
  let 
    dom = dominance g
    nodes = labNodes g
    domLabeled = map (\(a, b) -> (findNodeLabel a nodes, findNodeLabel b nodes)) dom
  in "digraph {\n" ++ concatMap (\(a, b) -> "  " ++ a ++ " -> " ++ b ++ ";\n") domLabeled ++ "}\n"

-- GraphViz the Control Flow Graph

nodeToDot :: LNode String -> String
nodeToDot (n, label) = "  " ++ show n ++ " [label=\"" ++ label ++ "\"];\n"

edgeToDot :: LEdge () -> String
edgeToDot (source, target, _) = "  " ++ show source ++ " -> " ++ show target ++ ";\n"

plotControlGraph :: Gr String () -> String
plotControlGraph graph = "digraph {\n" ++ nodes ++ edges ++ "}\n"
  where
    nodes = concatMap nodeToDot (labNodes graph)
    edges = concatMap edgeToDot (labEdges graph)

-- genAst :: [Ast.Function Range] -> String
-- genAst (a:_) = functions a
-- genAst [] = ""

-- functions :: Ast.Function Range -> String
-- functions (Ast.FunctionDef _ _ (Ast.GName _ name) _ blocks) = digraph (function (LBS.unpack name) blocks)
-- functions (Ast.FunctionDec _ _ (Ast.GName _ name) _) = show name
-- functions _ = "Unknown"

-- function :: String -> [Ast.BasicBlock Range] -> String
-- function name = pointFunction ("FuncDef " ++ name)

-- digraph :: String -> String
-- digraph content = "digraph {\n" ++ content ++ "}\n"

-- pointFunction :: String -> [Ast.BasicBlock Range] -> String
-- pointFunction name blocks = concatMap ((\stmt -> parentNode name ++ stmt) . statements) (concatMap extractStatements blocks)

-- extractStatements :: Ast.BasicBlock a -> [Ast.Stmt a]
-- extractStatements (Ast.BasicBlock _ _ _ stmts _) = stmts

-- parentNode :: String -> String
-- parentNode name = " \"" ++ name ++ "\" -> "

-- statements :: Ast.Stmt Range -> String
-- statements (Ast.SDec stmt) = parentNode "SDec" ++ decs stmt ++ "\n"
-- statements (Ast.SCall stmt) = parentNode "SCall" ++ calls stmt ++ "\n"
-- -- statements (Ast.SReturn stmt) = parentNode "SReturn" ++ returns stmt ++ "\n"

-- decs :: Ast.Dec Range -> String
-- decs (Ast.DecCall _ (Ast.LName _ name) call) = receiveBlock [show ("LName " ++ show name), calls call]
-- decs _ = "Unknown"

-- receiveBlock :: [String] -> String
-- receiveBlock x = "{" ++ receiveList x ++ "}"

-- receiveList :: [String] -> String
-- receiveList (a:x) = a ++ " " ++ receiveList x
-- receiveList [] = ""

-- calls :: Ast.Call Range -> String
-- calls (Ast.Call _ _ (Ast.GName _ name) _) = "\"call " ++ LBS.unpack name ++ "\""
-- calls _ = "Unknown"

-- returns :: Ast.Return Range -> String
-- returns (Ast.Return _ _ (Just valueReturned)) = show (value valueReturned)
-- returns (Ast.Return _ _ Nothing) = "void"

-- -- brs :: Ast.Br Range -> String
-- -- brs (Ast.Br _ _) = "br"

-- value :: Ast.Value Range -> String
-- value (Ast.ValueInt (Ast.IntegerValue _ int)) = "ValueInt" ++ show int
-- value (Ast.ValueName (Ast.LName _ name)) = "ValueName " ++ show name
-- value (Ast.ValueName (Ast.GName _ _)) = "Unknown"
