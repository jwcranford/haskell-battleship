-- Battleship game
module Battleship where

import Data.Ix 
import System.Random
import Data.Array
import Data.List
import Data.Monoid
import Control.Monad

data Ship = Ship { shipType:: Int, coords:: [(Int,Int)] } deriving Show

data Orientation = Vertical | Horizontal deriving (Enum, Show)

createShip :: Int -> Orientation -> (Int,Int) -> Ship
createShip size Vertical begin@(x,y)   = Ship size $ range (begin, (x,y+size-1))
createShip size Horizontal begin@(x,y) = Ship size $ range (begin, (x+size-1,y))

orientationFromInt :: Int -> Orientation
orientationFromInt x = toEnum $ x `mod` 2

randomOrientation :: IO Orientation
randomOrientation = do i <- getStdRandom random; return $ orientationFromInt i

standardBoardSize :: ((Int,Int),(Int,Int))
standardBoardSize = ((1,1), (10,10))

randomPlacement :: Int -> ((Int,Int),(Int,Int)) -> Orientation -> IO (Int,Int)
randomPlacement size ((minx,miny),(maxx, maxy)) Vertical = 
    do x <- getStdRandom $ randomR (minx, maxx)
       y <- getStdRandom $ randomR (miny, maxy - size + 1)
       return (x,y)
randomPlacement size ((minx,miny),(maxx, maxy)) Horizontal = 
    do x <- getStdRandom $ randomR (minx, maxx - size + 1)
       y <- getStdRandom $ randomR (miny, maxy)
       return (x,y)
       
-- places a ship of given size randomly on a grid of the given bounds
placeShipRandomly :: ((Int,Int),(Int,Int)) -> Int -> IO Ship
placeShipRandomly bnds size = 
    do ori <- randomOrientation
       p <- randomPlacement size bnds ori
       return $ createShip size ori p

placeStandardShipsRandomly :: IO [Ship]
placeStandardShipsRandomly = 
    sequence $ map (placeShipRandomly standardBoardSize) [2, 3, 3, 4, 5]

data Cell = Vacant (Maybe Bool) | Occupied Ship Bool | Collision Ship Ship

instance Show Cell where
    show (Vacant Nothing)      = "."
    show (Vacant (Just True))  = "x"
    show (Vacant (Just False)) = "o"
    show (Occupied _ True)     = "X"
    show (Occupied _ False)    = "O"
    show (Collision _ _)       = "*"

-- Cells can be combined as long as at least one is Vacant
instance Monoid Cell where
    mempty = Vacant Nothing

    mappend (Vacant ma) (Vacant mb) =
        Vacant $ liftM2 (\a b -> or [a, b]) ma mb
    mappend (Vacant _) occ@(Occupied _ _) = occ
    mappend occ@(Occupied _ _) (Vacant _) = occ
    mappend (Occupied s1 _) (Occupied s2 _) = Collision s1 s2
    mappend c@(Collision _ _) _ = c
    mappend _ c@(Collision _ _) = c

-- simple predicate for whether a cell is a collision or not
collision :: Cell -> Bool
collision (Collision _ _) = True
collision _ = False
    
data Board = Board { board :: Array (Int,Int) Cell
                    , shipTotal :: Int
                    , shipsSunk :: Int
                    , ships :: [Ship] } 

                    
-- takes a row index and the bounds of an 2-dimensional array and returns all the 
-- indices for the given row
rowIndices2D :: Int -> ((Int,Int),(Int,Int)) -> [(Int,Int)]
rowIndices2D i ((_,miny),(_,maxy)) = range ((i,miny),(i,maxy))                    
            
-- shows the given row of the array             
showsRowPrec :: (Show a) => Int -> Int -> Array (Int, Int) a -> ShowS
showsRowPrec prec row a = 
    let coords = rowIndices2D row (bounds a)
    in \s -> foldr (\i s1 -> ' ':(showsPrec prec (a!i) s1)) s coords

instance Show Board where
    showsPrec p b = 
        let a = board b
            ((minx,_),(maxx,_)) = bounds a
        in \so -> foldr (\row s -> showsRowPrec p row a ('\n':s)) so [minx..maxx]

        
createBoard :: ((Int,Int),(Int,Int)) -> [Ship] -> Board
createBoard _ [] = Board (array ((0,0),(0,0)) []) 0 0 []
createBoard bounds ss = 
    let assoc = [(cs, Occupied ship False) | ship <- ss, cs <- coords ship]
    in Board { board = accumArray mappend mempty bounds assoc,
               shipTotal = length ss,
               shipsSunk = 0,
               ships = ss }

createStandardBoard :: [Ship] -> Board
createStandardBoard = createBoard standardBoardSize

-- returns all colliding cells from the board
collisions :: Board -> [Cell]
collisions (Board b _ _ _) = filter collision (elems b)

createStandardRandomBoard :: IO Board
createStandardRandomBoard = 
    do ships <- placeStandardShipsRandomly
       return $ createStandardBoard ships

-- createValidBoard - repeats if collisions
-- createValidBoard :: [Ship]