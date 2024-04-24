
module Dominance (buildGraph, dominance) where

import qualified Ast
import Lexer

import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree
import TranslateAux (uname)
import Data.List (find)

buildGraph :: Ast.Function Range -> Gr String ()
buildGraph f = mkGraph nodes edges
  where
    nodes = getNodes f
    edges = getEdges f nodes

-- Function to extract edges from basic blocks of a function
getEdges :: Ast.Function Range -> [(Int, String)] -> [(Int, Int, ())]
getEdges (Ast.FunctionDef _ _ _ _ blocks) nodes = concatMap (getBlockEdges nodes) blocks
getEdges _ _ = []

-- Function to get edges from a basic block
getBlockEdges :: [(Int, String)] -> Ast.BasicBlock Range -> [(Int, Int, ())]
getBlockEdges nodes block =
  let nodeId = findNodeId nodes (getLabel block)
      successors = getBlockBranch block
      successorIds = map (findNodeId nodes) successors
  in map (\s -> (nodeId, s, ())) successorIds

-- Function to find the node ID of a basic block
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

getNodes :: Ast.Function a -> [(Int, String)]
getNodes (Ast.FunctionDef _ _ _ _ blocks) = zip [0..] (getLabelBlocks blocks)
getNodes _ = undefined

getLabelBlocks :: [Ast.BasicBlock a] -> [String]
getLabelBlocks = foldr (\ b -> (++) [getLabel b]) []

dominance :: [Ast.Function a] -> [String]
dominance = undefined

getLabel :: Ast.BasicBlock a -> String
getLabel (Ast.BasicBlock _ (Ast.LName _ name) _ _ _) = name
getLabel _ = undefined
