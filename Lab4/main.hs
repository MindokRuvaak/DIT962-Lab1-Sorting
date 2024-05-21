import Route
import RouteGUI
import Graph  -- Create a module and use a sensible graph representation
import Data.PSQueue (PSQ)
import qualified Data.PSQueue as PQ
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isNothing, fromJust)
import System.Environment


-- use the djukstra function to compute the set of nodes we can reach and the cost to do so 
-- and then compute the shortest path between two given nodes, returns nothing if there is no path
shortestPath :: (Ord a, Ord b, Num b) => Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to = findShortest from to (dijkstra g M.empty (PQ.singleton (from , from) 0))
 -- TODO: implement Dijkstra's algorithm

-- find the shortest path between two nodes given a set of Djikstra nodes
findShortest :: (Ord a, Ord b, Num b) => a -> a -> Map a (b, a) -> Maybe ([a],b)
findShortest from to set
  -- if the end node "to" is not present in the set we cannot reach it and return nothing
  | isNothing $ M.lookup to set = Nothing
  -- if the to node is present we compute the list of nodes taken to reach the end node
  | otherwise = Just (stops, totDist)
  where
    (totDist, _) = fromJust $ M.lookup to set
    stops = gatherStops [] from to set

-- compute the list of nodes needed to reach the end node
gatherStops :: Ord t => [t] -> t -> t -> Map t (a, t) -> [t]
gatherStops stops from to set
      | from == to = from:stops
      | otherwise  = let (_, prevTo) = fromJust $ M.lookup to set
      -- use reverse here to reduce constant factors, still O(n)
        in reverse (to : gatherStops stops from prevTo set)

dijkstra :: (Ord a, Ord b, Num b) => Graph a b -> Map a (b, a) -> PSQ (a, a) b -> Map a (b, a)
dijkstra g s q
  -- if queue is empty return the set of all Djikstra nodes
  | isNothing $ PQ.findMin q = s
  -- otherwise check if we already have seen the node
  | otherwise = if  x `M.notMember` s
    -- if we have'nt seen the node then we add it to the set of Djikstra nodes
    -- and add all adjacent nodes to the queue (as Djikstra nodes) 
    then dijkstra g (M.insert x (d, z) s) (foldr (insertQ . toKeyValuePair d) q' (adj x g))
    -- if we have seen the node already then do nothing
    else dijkstra g s q'
    where
      -- dequeue operation
      ((x , z) PQ.:->  d) = fromJust $ PQ.findMin q
      q' = PQ.deleteMin q

-- help function to convert the output of adj to Djikstra nodes
toKeyValuePair :: (Num b) => b -> Edge a b -> ((a,a), b)
toKeyValuePair d e = ((dst e, src e) , d + label e)

-- insert our touple of values into a PSQ correctly
insertQ :: (Ord a, Ord b) => ((a,a), b) -> PSQ (a,a) b -> PSQ (a,a) b
insertQ (value, prio) = PQ.insert value prio

graphBuilder :: [Stop] -> [LineStop] -> Graph String Integer
graphBuilder stops lineStops = graphBuilder' lineStops $ foldr (addVertex . name) Graph.empty stops
  where
    graphBuilder' :: [LineStop] -> Graph String Integer -> Graph String Integer
    graphBuilder' (x:y:ys) g  = graphBuilder' (y:ys) (addEdge (stopName x) (stopName y) (time y) g)
    graphBuilder' _  g        = g

outputParse :: Maybe([String], Integer) -> Maybe String
outputParse Nothing = Nothing
outputParse (Just(stops, time)) = Just $ show time ++ "\n" ++ unlines stops

main :: IO ()
main = do -- TODO: read arguments, build graph, output shortest path
  [stopsFile, linesFile, from, to] <- getArgs
  Right stops <- readStops ("data/" ++ stopsFile)
  Right lines <- readLines ("data/" ++ linesFile)
  let graph = graphBuilder stops (concatMap Route.stops lines)
  let rawOutput = shortestPath graph from to
  maybe (putStrLn "There is no path") print (outputParse rawOutput)
  return ()


startGUI :: IO ()
startGUI = do
  Right stops <- readStops "data/stops-air.txt"
  Right lines <- readLines "data/lines-air.txt"
  let graph = graphBuilder stops (concatMap Route.stops lines)
   -- TODO: build your graph here using stops and lines
  runGUI stops lines graph shortestPath

