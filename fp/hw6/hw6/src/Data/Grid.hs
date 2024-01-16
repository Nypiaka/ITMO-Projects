{-# LANGUAGE InstanceSigs #-}

-- | This module defines 'Grid' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.Grid
  ( Grid (..),
    gUp,
    gDown,
    gLeft,
    gRight,
  )
where

import Control.Comonad (Comonad (..))
import Data.ListZipper (ListZipper (..), lGenerator, lLeft, lRight)

gUp, gDown :: Grid a -> Grid a
gUp (Grid g) = Grid (lLeft g)
gDown (Grid g) = Grid (lRight g)

gLeft, gRight :: Grid a -> Grid a
gLeft (Grid g) = Grid (fmap lLeft g)
gRight (Grid g) = Grid (fmap lRight g)

gHorizontal, gVertical :: Grid a -> ListZipper (Grid a)
gHorizontal = lGenerator gLeft gRight
gVertical = lGenerator gUp gDown

newtype Grid a = Grid {unGrid :: ListZipper (ListZipper a)}

instance Functor Grid where
  fmap :: (a -> b) -> Grid a -> Grid b
  fmap f (Grid g) = Grid (fmap (fmap f) g)

instance Comonad Grid where
  extract :: Grid a -> a
  extract = extract . extract . unGrid
  duplicate :: Grid a -> Grid (Grid a)
  duplicate = Grid . fmap gHorizontal . gVertical
  extend :: (Grid a -> b) -> Grid a -> Grid b
  extend f = fmap f . duplicate
