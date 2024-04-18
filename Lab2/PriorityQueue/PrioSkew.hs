module PriorityQueue.PrioSkew
  ( delete,
    insert,
    SkewHeap,
    rootOf,
    {- merge, -}
  ) -- TODO: Add documentation and write down time complexity in big-O
where

-- this is a max heap
data SkewHeap a
  = Empty
  | Node (SkewHeap a) a (SkewHeap a)
  deriving (Show)

type Leaf = SkewHeap

-- dummy test data
test1, test2 :: SkewHeap Integer
test1 = Node (Node (Node Empty 1 Empty) 2 (Node Empty 1 Empty)) 3 Empty
test2 = Node Empty 6 (Node (Node Empty 4 Empty) 5 (Node Empty 4 Empty))

-- | merges two trees with a right biased skew merge algorithm
-- 
-- amortised O(log n)
merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge Empty t = t
merge t1@(Node l1 v1 r1) t2@(Node l2 v2 r2)
  | v1 > v2 = swapSubtrees $ Node l1 v1 (merge r1 t2)
  | otherwise = swapSubtrees $ Node l2 v2 (merge r2 t1)
  where
    swapSubtrees (Node l v r) = Node r v l

-- | removes an arbitrary element element
-- 
-- O(n log n)
delete :: Ord a => a -> SkewHeap a -> SkewHeap a
delete toDelete Empty = Empty
delete toDelete (Node l v r)
  | v == toDelete = merge l r
  | otherwise = Node (delete toDelete l) v (delete toDelete r)

-- | inserts an element into the heap using the skew merge
-- 
-- O(Log n)
insert :: Ord a => SkewHeap a -> a -> SkewHeap a
insert tree toInsert = merge tree (Node Empty toInsert Empty)

-- | gets the root elements value
-- 
-- O(1)
rootOf :: SkewHeap a -> Maybe a
rootOf Empty = Nothing
rootOf (Node _ v _) = Just v
