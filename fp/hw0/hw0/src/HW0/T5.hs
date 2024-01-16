module HW0.T5
  ( Nat,
    nFromNatural,
    nmult,
    nplus,
    ns,
    nToNum,
    nz,
  )
where

import Numeric.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ a = a

ns :: Nat a -> Nat a
ns prev f initial = f $ prev f initial

nplus :: Nat a -> Nat a -> Nat a
nplus a b f initial = a f $ b f initial

nmult :: Nat a -> Nat a -> Nat a
nmult a b f = a $ b f

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural num = ns $ nFromNatural $ num - 1

nToNum :: (Num a) => Nat a -> a
nToNum n = n (+ 1) 0
