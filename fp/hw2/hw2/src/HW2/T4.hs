module HW2.T4
  ( DotString (..),
    Fun (..),
    Inclusive (..),
    ListPlus (..),
  )
where

data ListPlus a = a :+ ListPlus a | Last a
  deriving (Show)

infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (listOneHead :+ listOneTail) listTwo = listOneHead :+ (listOneTail <> listTwo)
  (<>) (Last element) listTwo = element :+ listTwo

data Inclusive a b = This a | That b | Both a b
  deriving (Show)

-- You may necessary constraints there
instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) (Both firstA firstB) (Both secondA secondB) = Both (firstA <> secondA) (firstB <> secondB)
  (<>) (Both firstA firstB) (This secondA) = Both (firstA <> secondA) firstB
  (<>) (Both firstA firstB) (That secondB) = Both firstA (firstB <> secondB)
  (<>) (This firstA) (Both secondA firstB) = Both (firstA <> secondA) firstB
  (<>) (That firstB) (Both firstA secondB) = Both firstA (firstB <> secondB)
  (<>) (This firstA) (That firstB) = Both firstA firstB
  (<>) (That firstB) (This firstA) = Both firstA firstB
  (<>) (This firstA) (This secondA) = This (firstA <> secondA)
  (<>) (That firstB) (That secondB) = That (firstB <> secondB)

newtype DotString = DS String
  deriving (Show)

instance Semigroup DotString where
  (<>) (DS "") (DS stringTwo)        = DS stringTwo
  (<>) (DS stringOne) (DS "")        = DS stringOne
  (<>) (DS stringOne) (DS stringTwo) = DS (stringOne ++ "." ++ stringTwo)

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (<>) (F f1) (F f2) = F (f1 . f2)

instance Monoid (Fun a) where
  mempty = F id