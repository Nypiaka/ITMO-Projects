{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module HW6.T2
  ( TSet,
    Contains,
    Add,
    Delete,
  )
where

import Data.Type.Bool (If)
import GHC.TypeLits

type TSet = [Symbol]

type family Contains (name :: Symbol) (set :: TSet) :: Bool where
  Contains _ '[] = 'False
  Contains val (val ': _) = 'True
  Contains val (_ ': tail) = Contains val tail

type family Delete (name :: Symbol) (set :: TSet) :: TSet where
  Delete _ '[] = '[]
  Delete val (val ': tail) = Delete val tail
  Delete val (head ': tail) = head ': Delete val tail

type family Add (v :: Symbol) (set :: TSet) :: TSet where
  Add val set = If (Contains val set) set (val ': set)