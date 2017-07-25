-- Battleship game
module Battleship (Board(..)
  , Ship(..)
  , Cell(..)
  , shoot
  , applyShotResults
  , gameOver
  , emptyBoard
  , createBoard
  , standardBoardSize
  , createValidStandardRandomBoard
  , collisions
  ) where

import Data.Array
import Control.Monad
import Ship


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
rowIndices2D :: (Ix a,Ix b) => a -> ((a,b),(a,b)) -> [(a,b)]
rowIndices2D i ((_,miny),(_,maxy)) = range ((i,miny),(i,maxy))                    
            
-- shows the given row of the array 
showsRowPrec :: (Ix b, Show a) =>
                      Int -> Char -> Array (Char, b) a -> String -> [Char]
showsRowPrec prec row a = 
    let cs = rowIndices2D row (bounds a)
    in \s -> row : foldr (\i s1 -> ' ':(showsPrec prec (a!i) s1)) s cs

instance Show Board where
    showsPrec p b = 
        let a = board b
            ((minx,_),(maxx,_)) = bounds a
            ssunk = shipsSunk b
        in \so -> 
          let tl = shows ssunk (" ships sunk\n" ++ so)
          in foldr (\row s -> showsRowPrec p row a ('\n':s)) tl [minx..maxx]

        
createBoard :: (BoardIx,BoardIx) -> [Ship] -> Board
createBoard bs ss = 
    let assoc = [(cs, Occupied ship False) | ship <- ss, cs <- coords ship]
    in Board { board = accumArray mappend mempty bs assoc,
               shipTotal = length ss,
               shipsSunk = 0,
               ships = ss }

emptyBoard :: (BoardIx, BoardIx) -> Int -> Board
emptyBoard bs numShips = 
  (createBoard bs []) { shipTotal = numShips }

createStandardBoard :: [Ship] -> Board
createStandardBoard = createBoard standardBoardSize

-- returns all colliding cells from the board
collisions :: Board -> [Cell]
collisions (Board b _ _ _) = filter collision (elems b)

-- createValidStandardRandomBoard - repeats if collisions
createValidStandardRandomBoard :: IO Board
createValidStandardRandomBoard =
    do shipsPlaced <- placeStandardShipsRandomly;
       let b = createStandardBoard shipsPlaced
       if null $ collisions b
       then return b
       else createValidStandardRandomBoard


-- mapCell :: Ix i => i -> (a -> a) -> Array i a -> Array i a
-- mapCell i f a = a // [(i, f (a ! i))]

sunk :: Ship -> Array BoardIx Cell -> Bool
sunk s a =
  let cells = map (a !) $ coords s
      statii = map hit cells
  in and statii
 where hit (Occupied _ b) = b 
       hit _ = False

shoot ::  BoardIx -> Board -> (Bool,Board)
shoot cs b@(Board a total ssunk ss) =
  case a ! cs of
  (Vacant _) -> 
    let a' = a // [(cs,Vacant $ Just False)]
    in (False, b { board = a'})
  (Occupied _ True) -> (True, b)
  (Occupied ship False) -> 
    let a' = a // [(cs, Occupied ship True)]
    in if sunk ship a'
       then (True, Board a' total (ssunk + 1) ss)
       else (True, Board a' total ssunk ss)
  _ -> error (showString "Found a colliding cell in shoot: " $ show (a ! cs))


-- apply this to a vacant board
applyShotResults :: (BoardIx,Bool) -> Int -> Board -> Board
applyShotResults (cs,hit) ssunk (Board a t _ ss) =
  let a' = a // [(cs,Vacant $ Just hit)]
  in Board a' t ssunk ss


gameOver :: Board -> Bool
gameOver (Board _ tot ssunk _) = tot == ssunk
  
-- fmap with args reversed. <&> has the same relationship to <$> as & has to $  
-- infixl 5 <&>
-- (<&>) :: (Functor f) => f a -> (a -> b) -> f b
-- (<&>) = flip fmap

