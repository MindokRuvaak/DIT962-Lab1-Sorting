import Route
import RouteGUI
import Graph  -- Create a module and use a sensible graph representation
import PrioSkew
import qualified PrioSkew as PQ -- OBS: a max-heap
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isNothing, fromJust)

data Dijk a b
  = Dijk {
    to :: a,
    dist :: b,
    from :: a} deriving (Eq, Show)

instance (Ord a, Ord b) => Ord (Dijk a b) where
  compare d1 d2 = compare (dist d2) (dist d1)
-- higher priority on shorter distance


shortestPath :: (Ord a, Ord b, Num b) => Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to = convert $ dijkstra g M.empty (PQ.insert (Dijk from 0 from) PQ.empty)
 -- TODO: implement Dijkstra's algorithm

convert :: Map a (b,a) -> Maybe ([a],b)
convert = undefined

dijkstra :: (Ord a, Ord b) => Graph a b -> Map a (b, a) -> SkewHeap (Dijk a b) -> Map a (b, a)
dijkstra g s q
  | isNothing $ PQ.rootOf q = s
  | otherwise = if  x `M.notMember` s
    then dijkstra g (M.insert x (d, z) s) (foldr (insert . toDijk) q' (adj x g))
    else dijkstra g s q'
    where
      (Dijk x d z) = fromJust $ PQ.rootOf q
      q' = PQ.removeRoot q


toDijk :: Edge a b -> Dijk a b
toDijk e = Dijk (dst e) (label e) (src e)


main :: IO ()
main = undefined  -- TODO: read arguments, build graph, output shortest path

startGUI :: IO ()
startGUI = do
  Right stops <- readStops "your-stops.txt"
  Right lines <- readLines "your-lines.txt"
  let graph = undefined -- TODO: build your graph here using stops and lines
  runGUI stops lines graph shortestPath