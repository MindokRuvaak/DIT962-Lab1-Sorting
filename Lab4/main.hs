import Route
import RouteGUI
import Graph  -- Create a module and use a sensible graph representation
import PrioSkew
import qualified PrioSkew as PQ
import Data.Map (Map)
import qualified Data.Map as M

data Dijk a b = Dijk 
  {to :: a,
  dist :: b,
  from :: a}

shortestPath :: Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to = convert $ dijkstra g s q -- TODO: implement Dijkstra's algorithm
  where 
     s = PQ.empty
     q = PQ.insert (toDijkstraElem ) PQ.empty

dijkstra :: Graph a b -> SkewHeap Dijk -> SkewHeap Dijk -> SkewHeap Dijk

main :: IO ()
main = undefined  -- TODO: read arguments, build graph, output shortest path

startGUI :: IO ()
startGUI = do
  Right stops <- readStops "your-stops.txt"
  Right lines <- readLines "your-lines.txt"
  let graph = undefined -- TODO: build your graph here using stops and lines
  runGUI stops lines graph shortestPath