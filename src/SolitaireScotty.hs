{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Battleship (Board, BoardIx,
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

applyShot :: BoardIx -> InternalGame -> InternalGame
applyShot cs (InternalGame game@(Game i sb) rb) = 
  let (hit, rb') = shoot cs rb
      sb' = applyShotResults (cs, hit) (shipsSunk rb') sb
  in InternalGame (Game i sb') rb'

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
      Just ig ->
        -- update game
        let ig'@(InternalGame g _) = applyShot (r,c) ig
            updateGame gs2 = (Map.insert i ig' gs2, ())
        in do
          liftIO $ atomicModifyIORef' ref $ updateGame
          json g
      Nothing   -> status notFound404
    
