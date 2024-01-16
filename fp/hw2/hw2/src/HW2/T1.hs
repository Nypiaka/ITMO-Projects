module HW2.T1
  ( Tree (..),
    tfoldr,
  )
where

data Tree a = Leaf | Branch !Int (Tree a) a (Tree a)
  deriving (Show)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr func defaultValue (Branch _ left element right) = tfoldr func (func element (tfoldr func defaultValue right)) left
tfoldr _ defaultValue Leaf = defaultValue