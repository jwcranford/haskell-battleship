-- Battleship game
module Battleship where

import Data.Ix 
import System.Random
import Data.Array
import Data.List
import Data.Monoid
import Control.Monad

type BoardIx = (Int,Int)
data Ship = Ship { shipType:: String, numHits:: Int, coords:: [BoardIx] } deriving (Show, Eq)

data Orientation = Vertical | Horizontal deriving (Enum, Show)

createShip :: String -> Int -> Orientation -> BoardIx -> Ship
createShip name size Vertical begin@(x,y)   = Ship name size $ range (begin, (x,y+size-1))
createShip name size Horizontal begin@(x,y) = Ship name size $ range (begin, (x+size-1,y))

orientationFromInt :: Int -> Orientation
orientationFromInt x = toEnum $ x `mod` 2

randomOrientation :: IO Orientation
randomOrientation = do i <- getStdRandom random; return $ orientationFromInt i

standardBoardSize :: (BoardIx,BoardIx)
standardBoardSize = ((1,1), (10,10))

randomPlacement :: Int -> (BoardIx,BoardIx) -> Orientation -> IO BoardIx
randomPlacement size ((minx,miny),(maxx, maxy)) Vertical = 
    do x <- getStdRandom $ randomR (minx, maxx)
       y <- getStdRandom $ randomR (miny, maxy - size + 1)
       return (x,y)
randomPlacement size ((minx,miny),(maxx, maxy)) Horizontal = 
    do x <- getStdRandom $ randomR (minx, maxx - size + 1)
       y <- getStdRandom $ randomR (miny, maxy)
       return (x,y)
       
-- places a ship of given size randomly on a grid of the given bounds
placeShipRandomly :: (BoardIx,BoardIx) -> (String, Int) -> IO Ship
placeShipRandomly bnds (name, size) = 
    do ori <- randomOrientation
       p <- randomPlacement size bnds ori
       return $ createShip name size ori p

placeStandardShipsRandomly :: IO [Ship]
placeStandardShipsRandomly = 
    sequence $ map (placeShipRandomly standardBoardSize) [("Destroyer",2), ("Cruiser",3), ("Submarine",3), ("Battleship",4), ("Carrier",5)]

data Cell = Vacant (Maybe Bool) 
        | Occupied Ship Bool 
        | Collision Ship Ship
        deriving Eq

instance Show Cell where
    show (Vacant Nothing)      = "."
    show (Vacant (Just True))  = "x"
    show (Vacant (Just False)) = "*"
    show (Occupied _ True)     = "X"
    show (Occupied _ False)    = "O"
    show (Collision _ _)       = "#"

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
    
data Board = Board { board :: Array BoardIx Cell
                    , shipTotal :: Int
                    , shipsSunk :: Int
                    , ships :: [Ship] } 

                    
-- takes a row index and the bounds of an 2-dimensional array and returns all the 
-- indices for the given row
rowIndices2D :: Int -> (BoardIx,BoardIx) -> [BoardIx]
rowIndices2D i ((_,miny),(_,maxy)) = range ((i,miny),(i,maxy))                    
            
-- shows the given row of the array 
showsRowPrec :: (Show a) => Int -> Int -> Array BoardIx a -> ShowS
showsRowPrec prec row a = 
    let coords = rowIndices2D row (bounds a)
    in \s -> foldr (\i s1 -> ' ':(showsPrec prec (a!i) s1)) s coords

instance Show Board where
    showsPrec p b = 
        let a = board b
            ((minx,_),(maxx,_)) = bounds a
            ssunk = shipsSunk b
        in \so -> 
          let tl = shows ssunk (" ships sunk\n" ++ so)
          in foldr (\row s -> showsRowPrec p row a ('\n':s)) tl [minx..maxx]

        
createBoard :: (BoardIx,BoardIx) -> [Ship] -> Board
createBoard bounds ss = 
    let assoc = [(cs, Occupied ship False) | ship <- ss, cs <- coords ship]
    in Board { board = accumArray mappend mempty bounds assoc,
               shipTotal = length ss,
               shipsSunk = 0,
               ships = ss }

emptyBoard bounds numShips = 
  (createBoard bounds []) { shipTotal = numShips }

createStandardBoard :: [Ship] -> Board
createStandardBoard = createBoard standardBoardSize

-- returns all colliding cells from the board
collisions :: Board -> [Cell]
collisions (Board b _ _ _) = filter collision (elems b)

createStandardRandomBoard :: IO Board
createStandardRandomBoard = 
    do ships <- placeStandardShipsRandomly
       return $ createStandardBoard ships

-- createValidStandardRandomBoard - repeats if collisions
createValidStandardRandomBoard :: IO Board
createValidStandardRandomBoard =
    do { ships <- placeStandardShipsRandomly;
           let b = createStandardBoard ships
           in if null $ collisions b
              then return b
              else createValidStandardRandomBoard }


mapCell :: Ix i => i -> (a -> a) -> Array i a -> Array i a
mapCell i f a = a // [(i, f (a ! i))]

sunk s a =
	let cells = map (a !) $ coords s
	    hit (Occupied _ b) = b
	    statii = map hit cells
	in and statii

shoot ::  BoardIx -> Board -> (Bool,Board)
shoot cs b@(Board a total shipsSunk ships) =
  case a ! cs of
  (Vacant _) -> 
    let a' = a // [(cs,Vacant $ Just False)]
    in (False, b { board = a'})
  (Occupied _ True) -> (True, b)
  (Occupied ship False) -> 
    let a' = a // [(cs, Occupied ship True)]
    in if sunk ship a'
       then (True, Board a' total (shipsSunk + 1) ships)
       else (True, Board a' total shipsSunk ships)


-- apply this to a vacant board
applyShotResults :: (BoardIx,Bool) -> Int -> Board -> Board
applyShotResults (cs,hit) shipsSunk (Board a t _ []) =
  let a' = a // [(cs,Vacant $ Just hit)]
  in Board a' t shipsSunk []

readCoord :: String -> BoardIx
readCoord = read

oneSolitaireMove :: (Board,Board) -> IO (Board,Board)
oneSolitaireMove (realB,shadB) =
  do { putStrLn $ show (realB,shadB);
       putStr "Move? ";
       cs <- fmap readCoord getLine;
       let { (hit,realB') = shoot cs realB;
             shadB' = applyShotResults (cs,hit) (shipsSunk realB') shadB }
       in return (realB', shadB') }
	
gameOver :: Board -> Bool
gameOver (Board _ tot sunk _) = tot == sunk

solitaireLoop :: (Board,Board) -> IO (Board,Board)
solitaireLoop args@(_,sb) =
  if gameOver sb
  then putStrLn "game over!" >> return args
  else oneSolitaireMove args 
       >>= solitaireLoop
  
-- fmap with args reversed. <&> has the same relationship to <$> as & has to $  
infixl 5 <&>
(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip fmap

firstSolMove rb =
  let shadB = emptyBoard standardBoardSize 5
  in oneSolitaireMove (rb,shadB)


solitaire = 
  createValidStandardRandomBoard 
  >>= firstSolMove
  >>= solitaireLoop
         
