module PrioSkew
  ( delete,
    insert,
    SkewHeap,
    rootOf,
    empty,
    removeRoot,
    skewLeaf,
    {- merge, -}
  )
where

-- import Test.QuickCheck
import Data.Maybe (isJust, fromJust, isNothing)

-- | this is a max heap (inverse heap) i.e. largest element in root
data SkewHeap a
  = Empty
  | Node (SkewHeap a) a (SkewHeap a)

instance (Ord a, Show a) => Show (SkewHeap a) where
  showsPrec = undefined
  showList = undefined
  show Empty = ""
  show (Node Empty v Empty) = show v
  show sh@(Node l v r) = show v ++ ", " ++ show (delete v sh)


skewLeaf :: Ord a => a -> SkewHeap a
skewLeaf x = Node Empty x Empty

empty :: Ord a => SkewHeap a
empty = Empty

-- dummy test data
test1, test2 :: SkewHeap Integer
test1 = Node (Node (Node Empty 1 Empty) 2 (Node Empty 1 Empty)) 3 Empty
test2 = Node Empty 6 (Node (Node Empty 4 Empty) 5 (Node Empty 4 Empty))

-- | merges two trees with a right biased skew merge algorithm
-- 
-- amortised O(log n)
merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge Empty t = t
merge t Empty = t
merge t1@(Node l1 v1 r1) t2@(Node l2 v2 r2)
  | v1 > v2 = Node (merge r1 t2) v1 l1
  | otherwise = Node (merge r2 t1) v2 l2

-- | removes an arbitrary (provided) element
-- 
-- O(n)
delete :: Ord a => a -> SkewHeap a -> SkewHeap a
delete toDelete Empty = Empty
delete toDelete (Node l v r)
  | v == toDelete = merge l r
  | v < toDelete = Node l v r
  | otherwise = Node (delete toDelete l) v (delete toDelete r)

-- | removes the root element (max)
-- 
-- O(log n)
removeRoot :: Ord a => SkewHeap a -> SkewHeap a
removeRoot Empty = Empty
removeRoot (Node l _ r) = merge l r


-- | inserts an element into the heap using the skew merge
-- 
-- O(Log n)
insert :: Ord a => a -> SkewHeap a -> SkewHeap a
insert toInsert tree = merge tree (Node Empty toInsert Empty)

-- | gets the root elements value
-- 
-- O(1)
rootOf :: SkewHeap a -> Maybe a
rootOf Empty = Nothing
rootOf (Node _ v _) = Just v

fromList :: Ord a => [a] -> SkewHeap a
fromList = foldr insert Empty


heapInvariant :: Ord a => SkewHeap a -> Bool
heapInvariant Empty = True
heapInvariant (Node l v r) = check v l && check v r && heapInvariant l && heapInvariant r
  where
    check val sh = isNothing (rootOf sh) || val >= fromJust (rootOf sh)

size :: Ord a => SkewHeap a -> Integer
size Empty = 0
size (Node l v r) = 1 + size l + size r

prop_Heap :: Ord a => [a] -> Bool
prop_Heap xs = heapInvariant (fromList xs)
