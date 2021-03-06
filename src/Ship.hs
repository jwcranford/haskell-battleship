{-# LANGUAGE DeriveGeneric #-}

module Ship where

import Data.Ix 
import System.Random
import GHC.Generics
import Data.Aeson (ToJSON)

type BoardIx = (Int,Int)

data Ship = Ship { shipType:: String, numHits:: Int, coords:: [BoardIx] } deriving (Show, Eq, Generic)

instance ToJSON Ship

data Orientation = Vert | Horiz deriving (Enum, Show)

createShip :: String -> Int -> Orientation -> BoardIx -> Ship
createShip name size Horiz begin@(x,y) =
  let y' = toEnum $ (fromEnum y) + size - 1
  in Ship name size $ range (begin, (x,y'))
createShip name size Vert begin@(x,y) = 
  let x' = toEnum $ (fromEnum x) + size - 1
  in Ship name size $ range (begin, (x',y))

randomOrientation :: IO Orientation
randomOrientation = do
  i <- randomRIO (0,1)
  return $ toEnum i

standardBoardSize :: (BoardIx,BoardIx)
standardBoardSize = ((0,0), (9,9))

randomPlacement :: Int -> (BoardIx,BoardIx) -> Orientation -> IO BoardIx
randomPlacement size ((minx,miny),(maxx, maxy)) Horiz = 
  let maxy' = toEnum $ (fromEnum maxy) - size + 1
  in do x <- getStdRandom $ randomR (minx, maxx)
        y <- getStdRandom $ randomR (miny, maxy')
        return (x,y)
randomPlacement size ((minx,miny),(maxx, maxy)) Vert = 
  let maxx' = toEnum $ (fromEnum maxx) - size + 1
  in do x <- getStdRandom $ randomR (minx, maxx')
        y <- getStdRandom $ randomR (miny, maxy)
        return (x,y)
       
-- places a ship of given size randomly on a grid of the given bounds
placeShipRandomly :: (BoardIx,BoardIx) -> (String, Int) -> IO Ship
placeShipRandomly bnds (name, size) = 
    do ori <- randomOrientation
       p <- randomPlacement size bnds ori
       return $ createShip name size ori p

standardShips :: [(String,Int)]
standardShips = [("Destroyer",2), ("Cruiser",3), ("Submarine",3), ("Battleship",4), ("Carrier",5)]

placeStandardShipsRandomly :: IO [Ship]
placeStandardShipsRandomly = 
    sequence $ map (placeShipRandomly standardBoardSize) standardShips
