module HW3.T4
  ( State (..),
    Prim (..),
    Expr (..),
    mapState,
    wrapState,
    joinState,
    modifyState,
    eval,
  )
where

import HW3.T1

newtype State s a = S {runS :: s -> Annotated s a}

mapState :: (a -> b) -> State s a -> State s b
mapState outF (S insideF) = S (mapAnnotated outF . insideF)

wrapState :: a -> State s a
wrapState a = S (a :#)

joinState :: State s (State s a) -> State s a
joinState (S f) = S (\arg -> let a :# s = f arg in runS a s)

modifyState :: (s -> s) -> State s ()
modifyState f = S (\s -> () :# f s)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (<*>) m st = S (\arg -> let a :# s = runS m arg in runS (mapState a st) s)

instance Monad (State s) where
  (>>=) m f = S (\arg -> let a :# s = runS m arg; funRes = f a in runS funRes s)

data Prim a
  = Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving (Show)

data Expr = Val Double | Op (Prim Expr)
  deriving (Show)

instance Num Expr where
  (+) x y = Op (Add x y)
  (-) x y = Op (Sub x y)
  (*) x y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  (/) x y = Op (Div x y)
  fromRational x = Val (fromRational x)


binaryOp :: Expr -> Expr -> (Double -> Double -> Prim Double) -> (Double -> Double -> Double) -> State [Prim Double] Double
binaryOp x y prim op = do l <- eval x
                          r <- eval y
                          modifyState (prim l r :)
                          return (op l r)

unaryOp :: Expr -> (Double -> Prim Double) -> (Double -> Double) -> State [Prim Double] Double
unaryOp x prim op = do l <- eval x
                       modifyState (prim l :)
                       return (op l)


eval :: Expr -> State [Prim Double] Double
eval (Op (Add x y)) = binaryOp x y Add (+)
eval (Op (Mul x y)) = binaryOp x y Mul (*)
eval (Op (Sub x y)) = binaryOp x y Sub (-)
eval (Op (Div x y)) = binaryOp x y Div (/)
eval (Op (Abs x))   = unaryOp x Abs abs
eval (Op (Sgn x))   = unaryOp x Sgn signum
eval (Val x)        = return x
