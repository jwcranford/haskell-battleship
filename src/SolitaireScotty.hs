{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Battleship (Board,
                   createValidStandardRandomBoard,
                   emptyStandardBoard, shoot, applyShotResults, shipsSunk)

import Data.IORef (newIORef, atomicModifyIORef', readIORef, IORef)
import qualified Data.Map.Strict as Map (empty, lookup, insert, Map)

-- ghc
import GHC.Generics (Generic)

-- mtl
import Control.Monad.Trans (liftIO)

-- scotty
import Web.Scotty (scotty, post, put, get, param, json, status)
import Network.HTTP.Types.Status (notFound404)

-- aeson
import Data.Aeson (ToJSON, FromJSON)

-- random
import System.Random (randomRIO)


----------------------------------------------------------------------

data Game = Game { handle :: Int,
                   board :: Board } deriving Generic
instance ToJSON Game

data InternalGame = InternalGame Game Board


main :: IO ()
main = do 
 ref <- newIORef Map.empty
 scotty 3000 $ do
  post "/game" $ do
    let s = emptyStandardBoard
    i <- liftIO $ randomRIO (0,100000)
    let g = Game i s
    b <- liftIO $ createValidStandardRandomBoard
    let ig = InternalGame g b
    liftIO $ atomicModifyIORef' ref $ \gs -> (Map.insert i ig gs, ())
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
    
