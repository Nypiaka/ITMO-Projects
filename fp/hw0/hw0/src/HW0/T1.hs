{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module HW0.T1
  ( type (<->) (Iso),
    assocEither,
    assocPair,
    distrib,
    flipIso,
    runIso,
  )
where

data a <-> b = Iso (a -> b) (b -> a)

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib eitherFst = case eitherFst of
  Left a -> (Left a, Left a)
  Right (b, c) -> (Right b, Right c)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso (\(a, (b, c)) -> ((a, b), c)) (\((a, b), c) -> (a, (b, c)))

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither =
  Iso
    ( \fstEither -> case fstEither of
        Left a -> Left (Left a)
        Right (Left b) -> Left (Right b)
        Right (Right c) -> Right c
    )
    ( \sndEither -> case sndEither of
        Right c -> Right (Right c)
        Left (Left a) -> Left a
        Left (Right b) -> Right (Left b)
    )
