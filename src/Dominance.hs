
module Dominance (buildGraph, dominance, buildGraphs, dominances) where

import qualified Ast
import Lexer

import TranslateAux (uname)
import AstHelpers (getLabel)

import Data.Graph.Inductive.PatriciaTree ( Gr )
import Data.List (find)
import Data.Graph.Inductive.Query.Dominators (iDom)
import Data.Graph.Inductive.Graph (mkGraph, labNodes)

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.ByteString.Lazy.Char8 (ByteString)

dominance :: Gr ByteString () -> Gr ByteString ()
dominance g = let
    domination = iDom g 0
    nodes = labNodes g
    invertDom = map (\(a, b) -> (b, a)) domination
  in buildDomGraph invertDom nodes

dominances :: [Gr ByteString ()] -> [Gr ByteString ()]
dominances = map dominance

buildDomGraph :: [(Int, Int)] -> [(Int, ByteString)] -> Gr ByteString ()
buildDomGraph dom nodes = mkGraph nodes (map (\(a, b) -> (a, b, ())) dom)

buildGraphs :: Ast.Program Range -> [Gr ByteString ()]
buildGraphs (Ast.Program fs) = map buildGraph fs

buildGraph :: Ast.Function Range -> Gr ByteString ()
buildGraph f = mkGraph nodes edges
  where
    nodes = getNodes f
    edges = getEdges f nodes

getEdges :: Ast.Function Range -> [(Int, ByteString)] -> [(Int, Int, ())]
getEdges (Ast.FunctionDef _ _ _ _ blocks) nodes = concatMap (getBlockEdges nodes) blocks
-- getEdges _ _ = []

getBlockEdges :: [(Int, ByteString)] -> Ast.BasicBlock Range -> [(Int, Int, ())]
getBlockEdges nodes block =
  let nodeId = findNodeId nodes (getLabel block)
      successors = getBlockBranch block
      successorIds = map (findNodeId nodes) successors
  in map (\s -> (nodeId, s, ())) successorIds

findNodeId :: [(Int, ByteString)] -> ByteString -> Int
findNodeId nodes name =
  case find (\(_, nodeName) -> name == nodeName) nodes of
                 Just (nodeId, _) -> nodeId
                 Nothing          -> error $ "Node ID not found for block: " ++ LBS.unpack name

getBlockBranch :: Ast.BasicBlock Range -> [ByteString]
getBlockBranch (Ast.BasicBlock _ _ _ _ flow) = getBranch flow
-- getBlockBranch _ = []

getBranch :: Ast.Flow Range -> [ByteString]
getBranch (Ast.FlowBranch (Ast.Br _ [goto])) = [uname goto]
getBranch (Ast.FlowBranch (Ast.Br _ (_:goto1:goto2:_))) = [uname goto1, uname goto2]
getBranch _ = []

getNodes :: Ast.Function Range -> [(Int, ByteString)]
getNodes (Ast.FunctionDef _ _ _ _ blocks) = zip [0..] (getLabelBlocks blocks)
-- getNodes _ = undefined

getLabelBlocks :: [Ast.BasicBlock Range] -> [ByteString]
getLabelBlocks = foldr (\ b -> (++) [getLabel b]) []
