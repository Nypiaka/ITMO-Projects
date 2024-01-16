{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  ) where

import Numeric.Natural (Natural)
import Control.Applicative
import Control.Monad
import Data.Char

import HW4.Types
import HW4.T1 (ExceptState(..))
import qualified Data.Maybe

newtype ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P (ES fun)) s = case fun (0, s) of
  Success (a :# _) -> Success a
  Error e -> Error e

-- Just an example of parser that may be useful
-- in the implementation of 'parseExpr'
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES $ \(pos, _) ->
  Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (<|>) (P (ES pA)) (P (ES pB)) = P (ES (\(num, str)-> case pA (num, str) of
    Success (a :# sA) -> Success (a :# sA)
    Error _ -> case pB (num, str) of
      Success (b :# sB) -> Success (b :# sB)
      Error e -> Error e))

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) ->
  case s of
    ""     -> Success (() :# (pos, s))
    _      -> Error (ErrorAtPos pos)

parseInt :: Parser String
parseInt = some (mfilter isDigit pChar)

skipWs :: Parser ()
skipWs = void $ many (mfilter isSpace pChar)

checkChar :: Char -> Parser ()
checkChar char = void $ mfilter (== char) pChar

parseDouble :: String -> String -> Double
parseDouble intPart fracPart =
            let wholePart = foldl (\acc x -> acc * 10 + fromIntegral (digitToInt x)) 0 intPart
                fractionalPart = foldr (\x acc -> (acc + fromIntegral (digitToInt x)) / 10) 0 fracPart
            in wholePart + fractionalPart

pAftDot :: Parser (Maybe String)
pAftDot = optional $ do
  checkChar '.'
  parseInt

parseVal :: Parser Expr
parseVal = do
  skipWs
  n <- parseInt
  Val . parseDouble n . Data.Maybe.fromMaybe "0" <$> pAftDot

pExpr :: Parser Expr
pExpr = do
  skipWs
  checkChar '('
  e <- pE
  checkChar ')'
  return e

pSignAbstract :: Char -> (Expr -> Expr -> Prim Expr) -> Parser Expr -> Expr -> (Expr -> Parser Expr) -> Parser Expr
pSignAbstract op constructor parser l pexpr = do
  checkChar op
  term <- parser
  ans <- pexpr term
  return (Op (constructor l ans))

pPlus :: Expr -> Parser Expr
pPlus l = pSignAbstract '+' Add pT l pR

pMinus :: Expr -> Parser Expr
pMinus l = pSignAbstract '-' Sub pT l pR

pStar :: Expr -> Parser Expr
pStar l = pSignAbstract '*' Mul pF l pZ

pSlash :: Expr -> Parser Expr
pSlash l = pSignAbstract '/' Div pF l pZ

pF :: Parser Expr
pF = skipWs >> parseVal <|> pExpr

pZ :: Expr -> Parser Expr
pZ l = skipWs >> pStar l <|> pSlash l <|> pure l

pR :: Expr -> Parser Expr
pR l = skipWs >> pPlus l <|> pMinus l <|> pure l

pT :: Parser Expr
pT =  skipWs >> pF >>= pZ

pE :: Parser Expr
pE = skipWs >> pT >>= pR

start :: Parser Expr
start = do
    expr <- pE
    _    <- pEof
    return expr

-- No metohds
instance MonadPlus Parser

parseExpr :: String -> Except ParseError Expr
parseExpr = runP start
