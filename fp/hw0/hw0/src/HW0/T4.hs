module HW0.T4
  ( fac,
    fib,
    map',
    repeat',
  )
where

import Data.Function (fix)
import Numeric.Natural (Natural)

repeat' :: a -> [a]
repeat' x = fix (x :)

map' :: (a -> b) -> [a] -> [b]
map' =
  fix $ \myMap fn lst -> case lst of [] -> []; h : t -> fn h : myMap fn t

fib :: Natural -> Natural
fib =
  fix
    (\fb num prev count -> case count of 0 -> prev; 1 -> num; _ -> fb (num + prev) num (count - 1))
    1
    0

fac :: Natural -> Natural
fac =
  fix $ \fact num -> case num of 0 -> 1; _ -> num * fact (num - 1)
