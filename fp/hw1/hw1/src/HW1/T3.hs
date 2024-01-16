{-# LANGUAGE BlockArguments #-}

module HW1.T3
  ( Tree (..),
    tsize,
    tdepth,
    tmember,
    tinsert,
    tFromList,
  )
where

data Meta = M Int Int deriving (Show)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a) deriving (Show)

getElem :: Tree a -> a
getElem (Branch _ _ element _) = element

tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch (M size _) _ _ _) = size

tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch (M _ depth) _ _ _) = depth

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember element (Branch _ left currentElement right) =
  (element == currentElement) || tmember element (ternary (element > currentElement) right left)

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert element tree
  | tmember element tree = tree
  | otherwise = insertWithoutIsPresentCheck element tree

ternary :: Bool -> a -> a -> a
ternary cond first second = if cond then first else second

insertWithoutIsPresentCheck :: Ord a => a -> Tree a -> Tree a
insertWithoutIsPresentCheck element Leaf = Branch (M 1 1) Leaf element Leaf
insertWithoutIsPresentCheck element tree =
  rebalanced
    let cond = element < getElem tree
     in let x = insertWithoutIsPresentCheck element (ternary cond getLeft getRight tree)
         in treeCustomConstructor (ternary cond x (getLeft tree)) (getElem tree) (ternary cond (getRight tree) x)

diffL :: Tree a -> Int
diffL (Branch _ left _ right) = tdepth left - tdepth right

rebalanced :: Ord a => Tree a -> Tree a
rebalanced tree
  | diffL tree == -2 && (diffL (getRight tree) == -1 || diffL (getRight tree) == 0) = rotateLeft tree
  | diffL tree == 2 && (diffL (getLeft tree) == 1 || diffL (getLeft tree) == 0) = rotateRight tree
  | diffL tree == -2 && diffL (getRight tree) == 1 && (abs (diffL (getLeft (getRight tree))) <= 1) = bigRotateLeft tree
  | diffL tree == 2 && diffL (getLeft tree) == -1 && (abs (diffL (getRight (getLeft tree))) <= 1) = bigRotateRight tree
  | otherwise = tree

getLeft :: Tree a -> Tree a
getLeft (Branch _ left _ _) = left

getRight :: Tree a -> Tree a
getRight (Branch _ _ _ right) = right

bigRotateLeft :: Ord a => Tree a -> Tree a
bigRotateLeft (Branch meta left thisElement right) =
  rotateLeft (Branch meta left thisElement (rotateRight right))

bigRotateRight :: Ord a => Tree a -> Tree a
bigRotateRight (Branch meta left thisElement right) =
  rotateRight (Branch meta (rotateLeft left) thisElement right)

rotateLeft :: Ord a => Tree a -> Tree a
rotateLeft (Branch _ left element (Branch (M _ _) leftRight rightElement rightRight)) =
  treeCustomConstructor (treeCustomConstructor left element leftRight) rightElement rightRight

rotateRight :: Ord a => Tree a -> Tree a
rotateRight (Branch _ (Branch (M _ _) leftLeft leftElement rightLeft) element right) =
  treeCustomConstructor leftLeft leftElement (treeCustomConstructor rightLeft element right)

treeCustomConstructor :: Ord a => Tree a -> a -> Tree a -> Tree a
treeCustomConstructor left element right =
  Branch
    (M (1 + tsize left + tsize right) (1 + max (tdepth left) (tdepth right)))
    left
    element
    right

tFromList :: Ord a => [a] -> Tree a
tFromList list = fromListAndTree list Leaf

fromListAndTree :: Ord a => [a] -> Tree a -> Tree a
fromListAndTree list tree =
  ternary (null list) tree (fromListAndTree (tail list) (tinsert (head list) tree))
