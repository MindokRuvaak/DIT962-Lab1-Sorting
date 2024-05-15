{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Graph
  ( -- * Edge
    Edge                    -- type
  , src, dst, label         -- querying an Edge

    -- * Graph
  , Graph                   -- type
  , empty                   -- create an empty map
  , addVertex, addVertices  -- adding vertices (nodes)
  , addEdge, addBiEdge      -- adding edges (one way or both ways)
  , adj                     -- get adjacent nodes for a given node
  , vertices, edges         -- querying a Graph
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

-- An edge with a source and destination node (of type a), 
-- together with a label of type b
data Edge a b = Edge
  { src   :: a  -- ^ Source vertex
  , dst   :: a  -- ^ Destination vertex
  , label :: b  -- ^ The label
  } deriving Show

-- A graph with nodes of type a and labels of type b.
-- TODO: implement a graph with adjacency lists, hint: use a Map.
data Graph a b = Graph {kvmap :: Map a [Edge a b] } deriving Show

-- | Create an empty graph
empty :: Graph a b
empty = Graph M.empty

-- | Add a vertex (node) to a graph
addVertex :: Ord a => a -> Graph a b -> Graph a b
addVertex v g = Graph (M.insert v [] (kvmap g))

-- | Add a list of vertices to a graph
addVertices :: Ord a => [a] -> Graph a b -> Graph a b
addVertices vs g = foldr addVertex g vs

-- | Add an edge to a graph, the first parameter is the start vertex (of type a), 
-- the second parameter the destination vertex, and the third parameter is the
-- label (of type b). If one or more Node is missing return the Graph unchanged
addEdge :: Ord a => a -> a -> b -> Graph a b -> Graph a b
addEdge v w l g
  -- if one or more of our Nodes are missing we simply do othing
  | isNothing (M.lookup v (kvmap g)) || isNothing (M.lookup w (kvmap g)) 
    = g
  -- replace the old key value pair with the updated one
  -- TODO: we would like some way to compare edges (Eq instance) so we can make
  -- sure we dont have duplicate edges here
  | otherwise = Graph (M.insert v (e:es) (kvmap g))
    where
      e = Edge v w l
      es = fromJust (M.lookup v (kvmap g))

-- | Add an edge from start to destination, but also from destination to start,
-- with the same label.
addBiEdge :: Ord a => a -> a -> b -> Graph a b -> Graph a b
addBiEdge v w l = addEdge v w l . addEdge w v l

-- | Get all adjacent Nodes for a given Nodes, given as a list of its outgoing Edges
adj :: Ord a => a -> Graph a b -> [Edge a b]
adj v g = fromJust (M.lookup v (kvmap g))


-- | Get all vertices (nodes) in a graph
vertices :: Graph a b -> [a]
vertices g = M.keys $ kvmap g 

-- | Get all edges in a graph
edges :: Graph a b -> [Edge a b]
edges g = concat $ M.elems $ kvmap g
