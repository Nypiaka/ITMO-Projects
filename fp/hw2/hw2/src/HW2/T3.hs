module HW2.T3
  ( epart,
    mcat,
  )
where

fromMaybe :: (Monoid a) => Maybe a -> a
fromMaybe (Just a) = a
fromMaybe _        = mempty

mcat :: (Monoid a) => [Maybe a] -> a
mcat = foldr ((<>) . fromMaybe) mempty

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart [] = (mempty, mempty)
epart ((Left elemA) : listTail) =
  let (firstTypeElem, secondTypeElem) = epart listTail
   in (elemA <> firstTypeElem, secondTypeElem)
epart ((Right elemB) : listTail) =
  let (firstTypeElem, secondTypeElem) = epart listTail
   in (firstTypeElem, elemB <> secondTypeElem)