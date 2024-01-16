module HW1.T2
  ( N (..),
    nplus,
    nmult,
    nsub,
    nFromNatural,
    nToNum,
    ncmp,
    nEven,
    nOdd,
    ndiv,
    nmod,
  )
where

import Data.Maybe
import Numeric.Natural

data N = Z | S N

isZ :: N -> Bool
isZ Z = True
isZ (S _) = False

getN :: N -> N
getN (S n) = n
getN Z = Z

addOne :: N -> N
addOne = S

nplus :: N -> N -> N
nplus a b
  | isZ a = b
  | isZ b = a
  | otherwise = nplus (S a) (getN b)

multHelper :: N -> N -> N -> N
multHelper a b_start b
  | isZ a = Z
  | isZ (getN a) = nplus b b_start
  | otherwise = multHelper (getN a) b_start (nplus b b_start)

nmult :: N -> N -> N
nmult a b = multHelper a b Z

nsub :: N -> N -> Maybe N
nsub a b
  | isZ a =
    if isZ b
      then Just Z
      else Nothing
  | isZ b = Just a
  | otherwise = nsub (getN a) (getN b)

ncmp :: N -> N -> Ordering
ncmp a b =
  if isJust (nsub a b)
    then
      if isZ (fromJust (nsub a b))
        then EQ
        else GT
    else LT

nFromNaturalHelper :: N -> Natural -> N
nFromNaturalHelper n natural
  | natural == 0 = n
  | otherwise = nFromNaturalHelper (addOne n) (natural - 1)

nToNumHelper :: Num a => N -> a -> a
nToNumHelper n natural
  | isZ n = natural
  | otherwise = nToNumHelper (getN n) natural + 1

nFromNatural :: Natural -> N
nFromNatural = nFromNaturalHelper Z

nToNum :: Num a => N -> a
nToNum n = nToNumHelper n 0

nEven :: N -> Bool
nEven n = isZ (nmod n (S (S Z)))

nOdd :: N -> Bool
nOdd n = not (nEven n)

ndiv :: N -> N -> N
ndiv a b
  | ncmp a b == EQ = S Z
  | ncmp a b == LT = Z
  | otherwise = S (ndiv (fromJust (nsub a b)) b)

nmod :: N -> N -> N
nmod a b
  | ncmp a b == EQ = Z
  | ncmp a b == LT = a
  | otherwise = nmod (fromJust (nsub a b)) b
