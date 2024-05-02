
module Dominance (buildGraph, dominance) where

import qualified Ast
import Lexer

import TranslateAux (uname)
import AstHelpers (getLabel)

import Data.Graph.Inductive.PatriciaTree
import Data.List (find)
import Data.Graph.Inductive.Query.Dominators (iDom)
import Data.Graph.Inductive.Graph (mkGraph, labNodes)

dominance :: Gr String () -> Gr String ()
dominance g = let
    domination = iDom g 0
    nodes = labNodes g
    invertDom = map (\(a, b) -> (b, a)) domination
  in buildDomGraph invertDom nodes

buildDomGraph :: [(Int, Int)] -> [(Int, String)] -> Gr String ()
buildDomGraph dom nodes = mkGraph nodes (map (\(a, b) -> (a, b, ())) dom)

buildGraph :: Ast.Function Range -> Gr String ()
buildGraph f = mkGraph nodes edges
  where
    nodes = getNodes f
    edges = getEdges f nodes

getEdges :: Ast.Function Range -> [(Int, String)] -> [(Int, Int, ())]
getEdges (Ast.FunctionDef _ _ _ _ blocks) nodes = concatMap (getBlockEdges nodes) blocks
getEdges _ _ = []

getBlockEdges :: [(Int, String)] -> Ast.BasicBlock Range -> [(Int, Int, ())]
getBlockEdges nodes block =
  let nodeId = findNodeId nodes (getLabel block)
      successors = getBlockBranch block
      successorIds = map (findNodeId nodes) successors
  in map (\s -> (nodeId, s, ())) successorIds

findNodeId :: [(Int, String)] -> String -> Int
findNodeId nodes name =
  case find (\(_, nodeName) -> name == nodeName) nodes of
                 Just (nodeId, _) -> nodeId
                 Nothing          -> error $ "Node ID not found for block: " ++ name

getBlockBranch :: Ast.BasicBlock Range -> [String]
getBlockBranch (Ast.BasicBlock _ _ _ _ (Just flow)) = getBranch flow
getBlockBranch _ = []

getBranch :: Ast.Flow Range -> [String]
getBranch (Ast.FlowBranch (Ast.Br _ [goto])) = [uname goto]
getBranch (Ast.FlowBranch (Ast.Br _ (_:goto1:goto2:_))) = [uname goto1, uname goto2]
getBranch _ = []

getNodes :: Ast.Function Range -> [(Int, String)]
getNodes (Ast.FunctionDef _ _ _ _ blocks) = zip [0..] (getLabelBlocks blocks)
getNodes _ = undefined

getLabelBlocks :: [Ast.BasicBlock Range] -> [String]
getLabelBlocks = foldr (\ b -> (++) [getLabel b]) []
