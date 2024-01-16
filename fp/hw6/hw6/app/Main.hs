module Main (main) where

import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Grid
import Data.List (intercalate)
import Data.ListZipper
import HW6.T3 (Cell (..), CellState (..), Config (..), simulate)
import Options.Applicative

data InputConfig = InputConfig
  { config :: Config,
    gridSize :: Int,
    iterations :: Int
  }
  deriving (Show)

parseInputConfig :: Parser InputConfig
parseInputConfig =
  InputConfig <$> parseConfig
    <*> Options.Applicative.option
      auto
      (long "grid-size" <> metavar "Output grid size" <> help "Immunity duration (int)")
    <*> Options.Applicative.option
      auto
      (long "iterations" <> metavar "The number of simulation iterations" <> help "Grid size (int)")

parseConfig :: Parser Config
parseConfig =
  Config
    <$> Options.Applicative.option
      auto
      (long "prob" <> metavar "Infection probability" <> help "Probability (double)")
    <*> Options.Applicative.option
      auto
      (long "incub" <> metavar "Incubation period duration" <> help "Incubation period (int)")
    <*> Options.Applicative.option
      auto
      (long "ill" <> metavar "Illness duration" <> help "Illness duration (int)")
    <*> Options.Applicative.option
      auto
      (long "immun" <> metavar "Immunity duration" <> help "Immunity duration (int)")

toList :: Int -> ListZipper a -> [a]
toList g (LZ ls x rs) = reverse (take g ls) ++ [x] ++ take g rs

showGrid :: Int -> Grid Cell -> String
showGrid size g = intercalate "\n" $ map (toList size) $ toList size $ unGrid $ fmap toChar g
  where
    toChar :: Cell -> Char
    toChar (Cell Healthy _) = '_'
    toChar (Cell (Infected _) _) = 'i'
    toChar (Cell (Ill _) _) = '#'
    toChar (Cell (Immune _) _) = '@'

main :: IO ()
main = do
  conf <- execParser $ info parseInputConfig fullDesc
  valid <- validateInputConfig conf
  when valid $
    let cnf = config conf
        grid = gridSize conf
        iters = iterations conf
        arr = simulate cnf
        subarr = take iters arr
        args = map (showGrid (div grid 2)) subarr
     in forM_ args $ \r -> liftIO $ do
          putStr r
          putStrLn ""
          putStrLn ""

validateInputConfig :: InputConfig -> IO Bool
validateInputConfig inp = do
  let conf = config inp
      grid = gridSize inp
      iters = iterations inp
  when (grid <= 0) $ putStrLn "Invalid grid (should be > 0)"
  when (iters <= 0) $ putStrLn "Invalid iterations (should be > 0)"
  gridCheck <- checker "Invalid prob (should be 0 <= _ <= 1)" (probability conf) (\n -> n >= 0 && n <= 1)
  incubPositive <- checker "Invalid incubation period (should be > 0)" (incubationPeriod conf) (> 0)
  illPositive <- checker "Invalid ill duration period (should be > 0)" (illnessDuration conf) (> 0)
  immunPositive <- checker "Invalid immun duration period (should be > 0)" (immunityDuration conf) (> 0)
  return (grid > 0 && iters > 0 && gridCheck && incubPositive && illPositive && immunPositive)

checker :: String -> a -> (a -> Bool) -> IO Bool
checker errorMessage val predicate = do
  unless (predicate val) $ putStrLn errorMessage
  return $ predicate val
