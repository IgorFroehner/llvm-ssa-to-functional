
module GraphViz (plotGraph) where

import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph (labNodes, labEdges, LNode, LEdge)

nodeToDot :: LNode String -> String
nodeToDot (n, label) = "  " ++ show n ++ " [label=\"" ++ label ++ "\"];\n"

edgeToDot :: LEdge () -> String
edgeToDot (source, target, _) = "  " ++ show source ++ " -> " ++ show target ++ ";\n"

plotGraph :: Gr String () -> String
plotGraph graph = "digraph {\n" ++ nodes ++ edges ++ "}\n"
  where
    nodes = concatMap nodeToDot (labNodes graph)
    edges = concatMap edgeToDot (labEdges graph)
