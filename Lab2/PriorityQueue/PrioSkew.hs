module PriorityQueue.PrioSkew
  ( delete,
    insert,
    {- merge, -}
  ) -- TODO: Add documentation and write down time complexity in big-O
where

data SkewHeap a
  = Empty
  | Node (SkewHeap a) a (SkewHeap a)
  deriving (Show)

type Leaf = SkewHeap

test1, test2 :: SkewHeap Integer
test1 = Node (Node (Node Empty 1 Empty) 2 (Node Empty 1 Empty)) 3 Empty
test2 = Node Empty 6 (Node (Node Empty 4 Empty) 5 (Node Empty 4 Empty))

merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge Empty t = t
merge t1@(Node l1 v1 r1) t2@(Node l2 v2 r2)
  | v1 > v2 = swapSubtrees $ Node l1 v1 (merge r1 t2)
  | otherwise = swapSubtrees $ Node l2 v2 (merge r2 t1)
  where
    swapSubtrees (Node l v r) = Node r v l

delete :: Ord a => SkewHeap a -> SkewHeap a
delete Empty        = Empty
delete (Node l v r) = merge l r

insert :: Ord a => SkewHeap a -> a -> SkewHeap a
insert tree toInsert = merge tree (Node Empty toInsert Empty)
