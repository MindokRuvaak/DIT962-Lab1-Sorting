{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

--------------------------------------------------------------------------------

module AATree
  ( AATree, -- type of AA search trees
    emptyTree, -- AATree a
    get, -- Ord a => a -> AATree a -> Maybe a
    insert, -- Ord a => a -> AATree a -> AATree a
    inorder, -- AATree a -> [a]
    remove, -- Ord a => a -> AATree a -> AATree a
    size, -- AATree a -> Int
    height, -- AATree a -> Int
    checkTree, -- Ord a => AATree a -> Bool
  )
where
import Test.QuickCheck ()

--------------------------------------------------------------------------------

-- AA search trees
data AATree a
  = Empty
  | Node Level (AATree a) a (AATree a)
  deriving (Eq, {- Show, -} Read)

instance Show a =>  Show (AATree a) where
  showsPrec = undefined
  show Empty = "Empty-Tree"
  show (Node k Empty v Empty) = "["++ show k ++":"++ show v++"]"
  show (Node k l v Empty)     = "(" ++ show l ++ ")" ++ "_/" ++ "["++ show k ++":"++ show v ++"]"
  show (Node k Empty v r)     = "["++ show k ++":"++ show v ++"]" ++ "\\_" ++ "(" ++ show r ++ ")"
  show (Node k l v r)         = "(" ++ show l ++ ")" ++ "_/" ++ "["++ show k ++":"++ show v ++"]" ++ "\\_" ++ "(" ++ show r ++ ")"
  showList = undefined


-- | return an empty tree
-- | O(1)
emptyTree :: AATree a
emptyTree = Empty

-- | return the value specified or Nothing if the value is not present
-- | O(log n) since its a modified BST
get :: Ord a => a -> AATree a -> Maybe a
get _ Empty = Nothing
get toGet (Node _ l v r) = case compare toGet v of
  LT -> get toGet l
  GT -> get toGet r
  EQ -> Just v

-- | split any 4 nodes into 2 nodes again
-- | O(1)
split :: AATree a -> AATree a
split x@(Node xk a xv y@(Node yk b yv z@(Node {}))) =
  if xk == yk && xk == level z
    then
      let newX = Node xk a xv b
       in Node (yk + 1) newX yv z
  else x -- not a 4 node
    -- if not a 4 node given, just return tree unchanged
-- if we dont have at least 3 nodes we cant have a 4 node and we do nothing
split tree = tree


-- | preform a skew on a trees root node
-- | O(1)
skew :: AATree a -> AATree a
-- if malformed 3-node
skew y@(Node yk x@(Node xk a xv b) yv c) =
  if yk == xk
    then
      let newY = Node yk b yv c
       in Node xk a xv newY
    else y
-- a malformed 4-node will be fixed by algorithm for malformed 3-node
skew a = a


-- | insert a value into the tree unless the value already exists
-- | O(log n)
insert :: Ord a => a -> AATree a -> AATree a
-- recursivley insert an element, skew it if it has a malformed node and split any 4-nodes
insert toInsert = split . skew . insert' toInsert
  where
    -- if there is no tree then make one
    insert' val Empty = leaf val
    -- insert the value as normal into a BST-tree
    insert' val (Node k l v r) = case compare val v of
      LT -> Node k (insert val l) v r
      EQ -> Node k l v r
      GT -> Node k l v (insert val r)

-- | gives all elements in a tree as a list in increasing order
-- | O(n)
inorder :: AATree a -> [a]
inorder Empty = []
inorder t = go t []
  where
    go Empty acc = acc
    go (Node _ l v r) acc = go l (v:go r acc)

-- | gives the number of elements in a tree
-- | O(n²)
size :: AATree a -> Int
size = length . inorder

-- | gives the height of the tree (as seen as a binary tree)
-- | O(n)
height :: AATree a -> Int
height Empty = 0
height (Node _ l _ r) = max (height l + 1) (height r + 1)

--------------------------------------------------------------------------------
-- | Optional function
-- | O(?)
remove :: Ord a => a -> AATree a -> AATree a
remove _ Empty = Empty
remove toRemove t@(Node k l v r) = case compare toRemove v of
  LT -> Node k (remove toRemove l) v r
  EQ -> error "remove not implemented"
  GT -> Node k l v (remove toRemove r)


--------------------------------------------------------------------------------
-- other help functions
type Level = Int

-- | gives the level of a node
-- | O(1)
level :: AATree a -> Level
level Empty = 0
level (Node k _ _ _) = k

-- | created a tree with only one node
-- | O(1)
leaf :: a -> AATree a
leaf v = Node 1 Empty v Empty

-- | creates a tree given a list of elements that can be ordered
-- | O(n)
fromList :: Ord a => [a] -> AATree a
fromList = foldr insert Empty
--------------------------------------------------------------  ------------------
-- | Check that an AA tree is ordered and obeys the AA invariants
-- | O(n²)
checkTree :: Ord a => AATree a -> Bool
checkTree root =
  isSorted (inorder root)
    && all checkLevels (nodes root)
  where
    nodes :: Ord a => AATree a -> [AATree a] 
    nodes x
      | isEmpty x = []
      | otherwise = x : nodes (leftSub x) ++ nodes (rightSub x)

-- | True if the given list is ordered
-- | O(n)
isSorted :: Ord a => [a] -> Bool
isSorted []   = True
isSorted [x]  = True
isSorted (x:y:ys) = (x <= y) && isSorted (y:ys)

-- | Check if the invariant is true for a single AA node
-- | O(1)
checkLevels :: AATree a -> Bool
checkLevels node =
  leftChildOK node &&
  rightChildOK node &&
  rightRightGrandchildOK node

-- | checks if the left node is less than the parent node
-- | O(1)
leftChildOK :: AATree a -> Bool
leftChildOK node = level node - level (leftSub node) == 1
-- leftChildOK Empty = True
-- leftChildOK (Node k l _ _) = k - level l == 1

-- | checks if the right child nodes level is one less than (2 node) or equal to (3 node) the given node
-- | O(1)
rightChildOK :: AATree a -> Bool
rightChildOK node = (level node - level (rightSub node)) `elem` [0,1]
-- rightChildOK Empty = True
-- rightChildOK (Node k _ _ r) = k - level r == 0 || k - level r == 1

-- | checks if the right child node is greater than the given node (or else it would be a 4 node)
-- | O(1)
rightRightGrandchildOK :: AATree a -> Bool
rightRightGrandchildOK node = level node > level (rightSub $ rightSub node)
-- rightRightGrandchildOK Empty = True
-- rightRightGrandchildOK (Node k _ _ Empty) = True
-- rightRightGrandchildOK (Node k _ _ (Node _ _ _ rr)) = k > level rr

-- | returns true if a given tree is empty
-- | O(1)
isEmpty :: AATree a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | returns the left subtree
-- | O(1)
leftSub :: AATree a -> AATree a
leftSub (Node _ l _ _) = l
leftSub Empty          = Empty

-- | returns the right subtree
-- | O(1)
rightSub :: AATree a -> AATree a
rightSub (Node _ _ _ r) = r
rightSub Empty          = Empty

--------------------------------------------------------------------------------

-- | AATree quickcheck property using our invariant function
-- | O(n²)
prop_AATree :: Ord a => [a] -> Bool
prop_AATree = checkTree . fromList