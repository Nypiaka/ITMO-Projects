module HW3.T2
  ( distOption
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , distExcept
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , distFun
  , wrapFun
  ) where

import HW3.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some a, Some b) = Some (a, b)
distOption _                = None

wrapOption :: a -> Option a
wrapOption = Some

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P leftA rightA, P leftB rightB) = P (leftA, leftB) (rightA, rightB)

wrapPair :: a -> Pair a
wrapPair element = P element element

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q firstA secondA thirdA fourthA, Q firstB secondB thirdB fourthB) = 
  Q (firstA, firstB) (secondA, secondB) (thirdA, thirdB) (fourthA, fourthB)

wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (firstA :# firstE, secondA :# secondE) = (firstA, secondA) :# (firstE <> secondE)

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _)                    = Error e
distExcept (Success _, Error e)            = Error e
distExcept (Success first, Success second) = Success (first, second)

wrapExcept :: a -> Except e a
wrapExcept = Success

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low a, Low b)       = Low (a, b)
distPrioritised (Low a, Medium b)    = Medium (a, b)
distPrioritised (Low a, High b)      = High (a, b)
distPrioritised (Medium a, Low b)    = Medium (a, b)
distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (Medium a, High b)   = High (a, b)
distPrioritised (High a, Low b)      = High (a, b)
distPrioritised (High a, Medium b)   = High (a, b)
distPrioritised (High a, High b)     = High (a, b)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> streamA, b :> streamB) = (a, b) :> distStream (streamA, streamB)

wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

concatList :: List a -> List a -> List a
concatList (a :. insideL) outsideL = a :. concatList insideL outsideL
concatList Nil listB               = listB

makeList :: a -> List b -> List (a, b)
makeList a (b :. l) = (a, b) :. makeList a l
makeList _ Nil      = Nil

distList :: (List a, List b) -> List (a, b)
distList (a :. listA, b :. listB) = 
  concatList ((a, b) :. makeList a listB) (distList (listA, b :. listB))
distList (_, Nil)                 = Nil
distList (Nil, _)                 = Nil

wrapList :: a -> List a
wrapList a = a :. Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F funA, F funB) = F (\i -> (funA i, funB i))

wrapFun :: a -> Fun i a
wrapFun a = F (const a)
