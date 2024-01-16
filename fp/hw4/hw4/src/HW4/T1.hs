{-# LANGUAGE InstanceSigs #-}
module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import HW4.Types
import Control.Monad (ap)

newtype ExceptState e s a
  = ES {runES :: s -> Except e (Annotated s a)}

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f es = ES $ \s -> case runES es s of
  Error e -> Error e
  Success (a :# resS) -> Success $ f a:# resS

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> Success $ a:#s

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES f) =  ES $ \arg -> case f arg of
  Success (a :# s) -> runES a s
  Error e -> Error e

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success (() :# f s)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap :: (a -> b) -> ExceptState e s a -> ExceptState e s b
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = ap

instance Monad (ExceptState e s) where
  (>>=) m f = ES $ \arg -> case runES m arg of
    Success (a :# s) -> let funRes = f a in runES funRes s
    Error e -> Error e

data EvaluationError = DivideByZero
  deriving Show

binaryOp :: (Double -> Bool) -> Expr -> Expr -> (Double -> Double -> Prim Double) -> (Double -> Double -> Double) -> ExceptState EvaluationError [Prim Double] Double
binaryOp predicate x y prim op = do
                        l <- eval x
                        r <- eval y
                        if predicate r
                        then throwExceptState DivideByZero
                        else do
                              modifyExceptState (prim l r :)
                              return (op l r)

unaryOp :: Expr -> (Double -> Prim Double) -> (Double -> Double) -> ExceptState EvaluationError [Prim Double] Double
unaryOp x prim op = do l <- eval x
                       modifyExceptState (prim l :)
                       return (op l)

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Op (Add x y)) = binaryOp (const False) x y Add (+)
eval (Op (Mul x y)) = binaryOp (const False) x y Mul (*)
eval (Op (Sub x y)) = binaryOp (const False) x y Sub (-)
eval (Op (Div x y)) = binaryOp (== 0) x y Div (/)
eval (Op (Abs x))   = unaryOp x Abs abs
eval (Op (Sgn x))   = unaryOp x Sgn signum
eval (Val x)        = return x
