module HW2.T2
  ( joinWith,
    splitOn,
  )
where

import           Data.List.NonEmpty (NonEmpty ((:|)))

splitOnWithList :: (Eq a) => a -> a -> NonEmpty [a] -> NonEmpty [a]
splitOnWithList separator element (listHead :| listTail)
  | element /= separator = (element : listHead) :| listTail
  | otherwise = [] :| (listHead : listTail)

splitOn :: (Eq a) => a -> [a] -> NonEmpty [a]
splitOn separator list = foldr (splitOnWithList separator) ([] :| mempty) list

joinWithUtility :: a -> [a] -> [a] -> [a]
joinWithUtility separator element list = (separator : element) ++ list

joinWith :: a -> NonEmpty [a] -> [a]
joinWith separator (listHead :| listTail) = listHead ++ foldr (joinWithUtility separator) [] listTail