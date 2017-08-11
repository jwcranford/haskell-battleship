module Main where

import Battleship
import Data.Array
import System.IO

readCoord :: (BoardIx, BoardIx) -> [Char] -> Maybe BoardIx 
readCoord b (c:d:_) = if inRange b (c,d)
                      then Just (c,d)
                      else Nothing
readCoord _ _ = Nothing


oneSolitaireMove :: (Board,Board) -> IO (Board,Board)
oneSolitaireMove (realB,shadB) =
  do putStrLn $ show shadB
     putStr "Move? "
     hFlush stdout
     let rdr = readCoord $ bounds $ board shadB
     mcs <- fmap rdr getLine;
     case mcs of
       Just cs ->
         let (hit,realB') = shoot cs realB;
             shadB' = applyShotResults (cs,hit) (shipsSunk realB') shadB 
         in return (realB', shadB') 
       _ -> do putStrLn "input error";
               oneSolitaireMove (realB,shadB)

solitaireLoop :: (Board,Board) -> IO Board
solitaireLoop args@(_,sb) =
  if gameOver sb
  then putStrLn "game over!" >> return sb
  else oneSolitaireMove args 
       >>= solitaireLoop

firstSolMove :: Board -> IO (Board, Board)
firstSolMove rb =
  let shadB = emptyBoard standardBoardSize $ length standardShips
  in oneSolitaireMove (rb,shadB)
 
solitaire :: IO Board
solitaire = 
  createValidStandardRandomBoard 
  >>= firstSolMove
  >>= solitaireLoop

main :: IO ()
main = solitaire >>= putStrLn . show

