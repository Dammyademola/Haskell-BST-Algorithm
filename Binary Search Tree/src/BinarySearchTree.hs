module BinarySearchTree
  ( BST(..), insert, bstLookup, toList, remove, removeIf, rotateLeft, rotateRight, rebalance, balanceFactor, height)
  where

data BST = InternalNode Int String BST BST | Leaf
  deriving (Show, Eq)

-- Insert Key Value Pair
insert :: (Int, String) -> BST -> BST
insert (key, val) tree =
  let (newTree, _) = rebalance (insertRaw (key, val) tree) (balanceFactor tree)
  in newTree

insertRaw :: (Int, String) -> BST -> BST
insertRaw (key, val) Leaf = InternalNode key val Leaf Leaf
insertRaw (key, val) (InternalNode k v left right)
  | key < k   = InternalNode k v (insertRaw (key, val) left) right
  | key > k   = InternalNode k v left (insertRaw (key, val) right)
  | otherwise = InternalNode k val left right  -- Updates the value at the key

-- Lookup Key and Value
bstLookup :: Int -> BST -> Maybe String
bstLookup _ Leaf = Nothing
bstLookup soughtKey (InternalNode key val left right)
  | soughtKey == key = Just val
  | soughtKey < key  = bstLookup soughtKey left
  | otherwise        = bstLookup soughtKey right

-- Ordered List
toList :: BST -> [(Int, String)]
toList Leaf = []
toList (InternalNode key val left right) = toList left ++ [(key, val)] ++ toList right

-- Remove Function
remove :: Int -> BST -> BST
remove key tree =
  let (newTree, _) = rebalance (removeRaw key tree) (balanceFactor tree)
  in newTree

removeRaw :: Int -> BST -> BST
removeRaw _ Leaf = Leaf
removeRaw key (InternalNode k v left right)
  | key < k   = InternalNode k v (removeRaw key left) right
  | key > k   = InternalNode k v left (removeRaw key right)
  | otherwise = removeNode k left right
removeRaw _ tree = tree  -- Return the original tree if key not found


-- Helper function to handle node removal logic
removeNode :: Int -> BST -> BST -> BST
removeNode _ Leaf right = right
removeNode _ left Leaf = left
removeNode k left right = case left of
  Leaf -> right  -- If left is Leaf, return the right subtree
  _    -> let (lk, lv, l') = findMax left  -- Safe to call findMax now
          in InternalNode lk lv l' right


findMax :: BST -> (Int, String, BST)
findMax Leaf = error "findMax called on empty tree"
findMax (InternalNode k v Leaf r) = (k, v, r)
findMax (InternalNode k v left right) =
  let (maxK, maxV, newRight) = findMax right
  in (maxK, maxV, InternalNode k v left newRight)

-- Remove nodes that satisfy a predicate with rebalancing
removeIf :: (Int -> Bool) -> BST -> BST
removeIf _ Leaf = Leaf
removeIf predicate node@(InternalNode k v left right)
  | predicate k = 
      let -- Remove current node and rebalance
          newNode = removeNode k left right
          -- Apply removeIf recursively to the result and rebalance
          rebalancedNewNode = rebalanceTree newNode
      in removeIf predicate rebalancedNewNode
  | otherwise =
      -- Apply removeIf to children, then rebalance current node
      let newLeft = removeIf predicate left
          newRight = removeIf predicate right
      in rebalanceTree (InternalNode k v newLeft newRight)
  where
    rebalanceTree tree = fst $ rebalance tree (balanceFactor tree)


-- Updated Rebalancing Function
rebalance :: BST -> Int -> (BST, Int)
rebalance Leaf _ = (Leaf, 0)
rebalance t@(InternalNode k v left right) balance
  | balance > 1 =
      if balanceFactor right < 0
      then let (rightRotated, _) = rotateRight right
           in let (resultTree, resultBalance) = rotateLeft (InternalNode k v left rightRotated)
              in (resultTree, resultBalance)
      else let (resultTree, resultBalance) = rotateLeft t
           in (resultTree, resultBalance)
  | balance < -1 =
      if balanceFactor left > 0
      then let (leftRotated, _) = rotateLeft left
           in let (resultTree, resultBalance) = rotateRight (InternalNode k v leftRotated right)
              in (resultTree, resultBalance)
      else let (resultTree, resultBalance) = rotateRight t
           in (resultTree, resultBalance)
  | otherwise = (t, balance)

rotateLeft :: BST -> (BST, Int)
rotateLeft (InternalNode k v left (InternalNode rk rv rl rr)) =
    let newTree = InternalNode rk rv (InternalNode k v left rl) rr
    in (newTree, balanceFactor newTree)
rotateLeft t = (t, balanceFactor t)

rotateRight :: BST -> (BST, Int)
rotateRight (InternalNode k v (InternalNode lk lv ll lr) right) =
    let newTree = InternalNode lk lv ll (InternalNode k v lr right)
    in (newTree, balanceFactor newTree)
rotateRight t = (t, balanceFactor t)

balanceFactor :: BST -> Int
balanceFactor Leaf = 0
balanceFactor (InternalNode _ _ left right) = height right - height left

height :: BST -> Int
height Leaf = 0
height (InternalNode _ _ left right) = 1 + max (height left) (height right)