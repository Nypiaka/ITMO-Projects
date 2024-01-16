{-# LANGUAGE InstanceSigs #-}

-- | This module defines 'ListZipper' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.ListZipper
  ( ListZipper (..),
    lLeft,
    lRight,
    lGenerator,
  )
where

import Control.Comonad (Comonad (..))

data ListZipper a = LZ [a] a [a]

instance Functor ListZipper where
  fmap :: (a -> b) -> ListZipper a -> ListZipper b
  fmap f (LZ ll e rl) = LZ (map f ll) (f e) (map f rl)

lGenerator :: (a -> a) -> (a -> a) -> a -> ListZipper a
lGenerator f g x = LZ (iterateTail f x) x (iterateTail g x)

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

lLeft, lRight :: ListZipper a -> ListZipper a
lLeft (LZ (l : ls) c rs) = LZ ls l (c : rs)
lLeft lz = lz
lRight (LZ ls c (r : rs)) = LZ (c : ls) r rs
lRight lz = lz

instance Comonad ListZipper where
  extract :: ListZipper a -> a
  extract (LZ _ x _) = x
  extend :: (ListZipper a -> b) -> ListZipper a -> ListZipper b
  extend f = fmap f . duplicate
  duplicate :: ListZipper a -> ListZipper (ListZipper a)
  duplicate = lGenerator lLeft lRight