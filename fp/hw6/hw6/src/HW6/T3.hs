module HW6.T3
  ( Config (..),
    Cell (..),
    CellState (..),
    Comonad19Grid,
    simulate,
  )
where

import Control.Comonad
import Control.Monad (liftM2)
import Data.Grid (Grid (..), gDown, gLeft, gRight, gUp)
import Data.ListZipper (ListZipper, lGenerator)
import System.Random (Random (random), StdGen, mkStdGen, uniformR)

data Config = Config
  { probability :: Double,
    incubationPeriod :: Int,
    illnessDuration :: Int,
    immunityDuration :: Int
  }
  deriving (Show)

data CellState
  = Healthy
  | Infected Int
  | Ill Int
  | Immune Int
  deriving (Show)

data Cell = Cell
  { cellState :: CellState,
    cellRand :: StdGen
  }

type Comonad19Grid = Grid Cell

creator :: Bool -> Cell -> Cell
creator flag p =
  let (a, _) = random (cellRand p)
   in Cell Healthy $ mkStdGen $ if flag then a else a + 9

generatorTemplate :: Cell -> ListZipper Cell
generatorTemplate = lGenerator (creator True) (creator False)

magicNum1 :: Int
magicNum1 = 41

magicNum2 :: Int
magicNum2 = 29

magicNum3 :: Int
magicNum3 = 35

simulate :: Config -> [Comonad19Grid]
simulate config =
  iterate
    (evolve config)
    ( Grid $
        lGenerator
          (\_ -> generatorTemplate (Cell Healthy $ mkStdGen magicNum1))
          (\_ -> generatorTemplate (Cell Healthy $ mkStdGen magicNum2))
          ( generatorTemplate (Cell (Infected 1) $ mkStdGen magicNum3)
          )
    )

mayInfect :: [Cell] -> Bool
mayInfect = any $ \cell -> case cellState cell of Ill _ -> True; Infected _ -> True; _ -> False

neighbors :: [Grid a -> Grid a]
neighbors = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where
    horizontals = [gLeft, gRight]
    verticals = [gUp, gDown]

infect :: Grid Cell -> Bool
infect g = mayInfect $ map (\direction -> extract $ direction g) neighbors

rule :: Config -> Grid Cell -> Cell
rule config g =
  if infect g
    then
      ( case cellState $ extract g of
          Healthy ->
            let (val, gn) = uniformR (0 :: Double, 1 :: Double) (cellRand $ extract g)
             in if val <= probability config
                  then Cell (Infected 1) gn
                  else Cell Healthy gn
          anoth -> Cell (transform config anoth) (cellRand $ extract g)
      )
    else Cell (transform config (cellState $ extract g)) (cellRand $ extract g)

transform :: Config -> CellState -> CellState
transform config cell =
  case cell of
    Infected num -> if num < incubationPeriod config then Infected (num + 1) else Ill 1
    Ill num -> if num < illnessDuration config then Ill (num + 1) else Immune 1
    Immune num -> if num < immunityDuration config then Immune (num + 1) else Healthy
    Healthy -> Healthy

evolve :: Config -> Grid Cell -> Comonad19Grid
evolve config = extend (rule config)