-- Battleship game

import Data.Ix 
import System.Random
import Data.Array

data Ship = Ship { shipType:: Int, coords:: [(Int,Int)] } deriving Show

data Orientation = Vertical | Horizontal deriving (Enum, Show)

createShip :: Int -> Orientation -> (Int,Int) -> Ship
createShip size Vertical begin@(x,y)   = Ship size $ range (begin, (x,y+size-1))
createShip size Horizontal begin@(x,y) = Ship size $ range (begin, (x+size-1,y))

orientationFromInt :: Int -> Orientation
orientationFromInt x = toEnum $ x `mod` 2

randomOrientation :: IO Orientation
randomOrientation = do i <- getStdRandom random; return $ orientationFromInt i

randomPlacement :: Int -> (Int, Int) -> Orientation -> IO (Int,Int)
randomPlacement size (maxx, maxy) Vertical = 
    do x <- getStdRandom $ randomR (1, maxx)
       y <- getStdRandom $ randomR (1, maxy - size + 1)
       return (x,y)
randomPlacement size (maxx, maxy) Horizontal = 
    do x <- getStdRandom $ randomR (1, maxx - size + 1)
       y <- getStdRandom $ randomR (1, maxy)
       return (x,y)
       
-- places a ship of given size randomly on a grid of the given bounds
placeShipRandomly :: (Int, Int) -> Int -> IO Ship
placeShipRandomly bnds size = 
    do ori <- randomOrientation
       p <- randomPlacement size bnds ori
       return $ createShip size ori p

placeStandardShipsRandomly :: IO [Ship]
placeStandardShipsRandomly = sequence $ map (placeShipRandomly (10,10)) [2, 3, 3, 4, 5]

data Cell = Vacant (Maybe Bool) | Occupied Ship Bool 

instance Show Cell where
    show (Vacant Nothing)      = "."
    show (Vacant (Just True))  = "x"
    show (Vacant (Just False)) = "o"
    show (Occupied _ True)     = "X"
    show (Occupied _ False)    = "O"
    
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

createStandardBoard :: [Ship] -> Board
createStandardBoard [] = Board (array ((0,0),(0,0)) []) 0 0 []
createStandardBoard ss = 
    let assoc = [(cs, Occupied ship False) | ship <- ss, cs <- coords ship]
    in Board { board = (accumArray (\_ a -> a) (Vacant Nothing) ((1,1), (10,10)) assoc),
               shipTotal = length ss,
               shipsSunk = 0,
               ships = ss }
