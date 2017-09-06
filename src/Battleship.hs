{-# LANGUAGE DeriveGeneric #-}

-- Battleship game
module Battleship (Board(..)
  , Ship(..)
  , Cell(..)
  , BoardIx
  , shoot
  , applyShotResults
  , gameOver
  , createBoard
  , createBoard'
  , emptyStandardBoard
  , createValidStandardRandomBoard
  , collisions
  , sunk
  ) where

import Ship
import Cell

import GHC.Generics (Generic)
import Data.Array

import Data.Aeson (ToJSON, toJSON, toEncoding, foldable)

----------------------------------------------------------------------
data Board = Board { board :: Array BoardIx Cell
                    , shipTotal :: Int
                    , shipsSunk :: [String]
                    , ships :: [Ship] } deriving Generic

instance (ToJSON a) => ToJSON (Array i a) where
  toJSON arr = toJSON $ elems arr
  toEncoding arr = foldable arr

instance ToJSON Board
                    
-- takes a row index and the bounds of an 2-dimensional array and
-- returns all the indices for the given row
rowIndices2D :: (Ix a,Ix b) => a -> ((a,b),(a,b)) -> [(a,b)]
rowIndices2D i ((_,miny),(_,maxy)) = range ((i,miny),(i,maxy))

-- shows the given row of the array 
showsRowPrec :: (Ix b, Show a, Ix c, Show c) =>
                      Int -> c -> Array (c, b) a -> String -> [Char]
showsRowPrec prec row a = 
    let cs = rowIndices2D row (bounds a)
    in \s -> show row ++ foldr (\i s1 -> ' ':(showsPrec prec (a!i) s1)) s cs

instance Show Board where
    showsPrec p b = 
        let a = board b
            ((minx,_),(maxx,_)) = bounds a
            ssunk = shipsSunk b
        in \so ->
          let tl = shows (length ssunk) (" ships sunk: " ++ show ssunk ++ '\n':so)
          in foldr (\row s -> showsRowPrec p row a ('\n':s)) tl [minx..maxx]


-- used for testing
createBoard' :: Bool -> (BoardIx,BoardIx) -> [Ship] -> Board
createBoard' isSunk bs ss =
  let assoc = [(cs, Occupied ship isSunk) | ship <- ss, cs <- coords ship]
  in Board { board = accumArray mappend mempty bs assoc,
             shipTotal = length ss,
             shipsSunk = if isSunk then map shipType ss else [],
             ships = ss }


createBoard :: (BoardIx,BoardIx) -> [Ship] -> Board
createBoard = createBoard' False
    
emptyBoard :: (BoardIx, BoardIx) -> Int -> Board
emptyBoard bs numShips = 
  (createBoard bs []) { shipTotal = numShips }

emptyStandardBoard :: Board
emptyStandardBoard =
  emptyBoard standardBoardSize $ length standardShips

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
       then (True, Board a' total ((shipType ship) : ssunk) ss)
       else (True, Board a' total ssunk ss)
  _ -> error (showString "Found a colliding cell in shoot: " $ show (a ! cs))


-- apply this to a vacant board
applyShotResults :: (BoardIx,Bool) -> [String] -> Board -> Board
applyShotResults (cs,hit) ssunk (Board a t _ ss) =
  let a' = a // [(cs,Vacant $ Just hit)]
  in Board a' t ssunk ss


gameOver :: Board -> Bool
gameOver (Board _ tot ssunk _) = tot == (length ssunk)
  
-- fmap with args reversed. <&> has the same relationship to <$> as & has to $  
-- infixl 5 <&>
-- (<&>) :: (Functor f) => f a -> (a -> b) -> f b
-- (<&>) = flip fmap

