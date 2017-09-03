{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Battleship (Board,
                   createValidStandardRandomBoard,
                   emptyStandardBoard, shoot, applyShotResults, shipsSunk)

import Data.IORef (newIORef, atomicModifyIORef', readIORef)
import qualified Data.Map.Strict as Map (empty, lookup, insert)

-- ghc
import GHC.Generics (Generic)

-- mtl
import Control.Monad.Trans (liftIO)

-- scotty
import Web.Scotty (scotty, post, put, get, param, json, status)
import Network.HTTP.Types.Status (notFound404)

-- aeson
import Data.Aeson (ToJSON)

-- random
import System.Random (randomRIO)


----------------------------------------------------------------------

-- A Game gets returned to the client, so the board is the shadow board
-- without any ships on it, just hits and misses.
data Game = Game { handle :: Int,
                   board :: Board } deriving Generic
instance ToJSON Game

-- An InternalGame is the Game that the client sees plus the actual board
-- with ships on it.
data InternalGame = InternalGame Game Board

newGame :: IO InternalGame
newGame = let s = emptyStandardBoard in do
  i <- randomRIO (0,100000)
  b <- createValidStandardRandomBoard
  return $ InternalGame (Game i s) b


main :: IO ()
main = do 
 ref <- newIORef Map.empty
 scotty 3000 $ do
  post "/game" $ do
    ig@(InternalGame g _) <- liftIO newGame
    liftIO $ atomicModifyIORef' ref $ \gs -> (Map.insert (handle g) ig gs, ())
    json g
  get "/game/:id" $ do
    i <- param "id"
    gs <- liftIO $ readIORef ref
    let migame = Map.lookup i gs
    case migame of
      Just (InternalGame game _) -> json game
      Nothing   -> status notFound404
  put "/game/:id/shoot/:row/:column" $ do
    i <- param "id"
    r <- param "row"
    c <- param "column"
--    liftIO $ print (r,c)
    gs <- liftIO $ readIORef ref
    let migame = Map.lookup i gs
    case migame of
      Just (InternalGame game rb) ->
        let (hit, rb') = shoot (r,c) rb
            sb' = applyShotResults ((r,c), hit) (shipsSunk rb') (board game)
            game' = Game i sb'
            ig' = InternalGame game' rb'
        -- update game
        in do
          liftIO $ atomicModifyIORef' ref $ \gs2 -> (Map.insert i ig' gs2, ())
          json game'
      Nothing   -> status notFound404
    
