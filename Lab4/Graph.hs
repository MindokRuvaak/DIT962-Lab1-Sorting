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
data Graph a b = Graph {edgeMap :: Map a [Edge a b] } deriving Show

-- | Create an empty graph
empty :: Graph a b
empty = Graph M.empty

-- | Add a vertex (node) to a graph
addVertex :: Ord a => a -> Graph a b -> Graph a b
addVertex v g = Graph (M.insert v [] (edgeMap g))

-- | Add a list of vertices to a graph
addVertices :: Ord a => [a] -> Graph a b -> Graph a b
addVertices vs g = foldr addVertex g vs

-- | Add an edge to a graph, the first parameter is the start vertex (of type a), 
-- the second parameter the destination vertex, and the third parameter is the
-- label (of type b)
addEdge :: Ord a => a -> a -> b -> Graph a b -> Graph a b
addEdge v w l g
  -- you need both the start and end node to exist to have a valid edge
  -- this also handle the case of an empty Graph
  | isNothing (M.lookup v (edgeMap g)) || isNothing (M.lookup w (edgeMap g)) 
    = error "vertex does not exist"
  -- replace the old key value pair with the updated one
  -- TODO: we would like some way to compare edges (Eq instance) so we can make
  -- sure we dont have duplicate edges here
  | otherwise = Graph (M.insert v (e:es) (edgeMap g))
    where
      e = Edge v w l
      es = fromJust (M.lookup v (edgeMap g))

-- | Add an edge from start to destination, but also from destination to start,
-- with the same label.
addBiEdge :: Ord a => a -> a -> b -> Graph a b -> Graph a b
addBiEdge v w l = addEdge v w l . addEdge w v l

-- | Get all adjacent Nodes for a given Nodes given as a list of its outgoing edges
adj :: Ord a => a -> Graph a b -> [Edge a b]
adj v g = fromJust (M.lookup v (edgeMap g))

-- | Get all vertices (nodes) in a graph
vertices :: Graph a b -> [a]
vertices g = undefined

-- | Get all edges in a graph
edges :: Graph a b -> [Edge a b]
edges g = undefined
