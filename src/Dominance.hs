
module Dominance (buildGraph, dominance) where

import qualified Ast

import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree

buildGraph :: Ast.Function a -> Gr String ()
buildGraph f = mkGraph nodes edges
  where
    nodes = getNodes f
    edges = []

getEdges :: Ast.Function a -> [(Int, Int)]
getEdges (Ast.FunctionDef _ _ _ _ blocks) = undefined
getEdges _ = undefined

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
