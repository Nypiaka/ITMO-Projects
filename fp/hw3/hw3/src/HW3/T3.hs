module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1

joinOption :: Option (Option a) -> Option a
joinOption None     = None
joinOption (Some a) = a

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e)   = Error e
joinExcept (Success a) = a

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# insideE) :# outsideE) = a :# (outsideE <> insideE)

concatList :: List a -> List a -> List a
concatList (a :. insideL) outsideL = a :. concatList insideL outsideL
concatList Nil l = l

joinList :: List (List a) -> List a
joinList (h :. t) = concatList h (joinList t)
joinList Nil = Nil

fromFun :: Fun i a -> (i -> a)
fromFun (F f) = f

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\i -> fromFun (f i) i)
