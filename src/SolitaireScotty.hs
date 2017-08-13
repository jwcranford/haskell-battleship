{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Battleship (Board,
                   createValidStandardRandomBoard,
                   emptyStandardBoard)

import Data.IORef (newIORef, atomicModifyIORef', readIORef)
import qualified Data.Map.Strict as Map (empty, lookup, insert)

-- ghc
import GHC.Generics (Generic)

-- mtl
import Control.Monad.Trans (liftIO)

-- scotty
import Web.Scotty (scotty, post, get, param, json, status)
import Network.HTTP.Types.Status (notFound404)

-- aeson
import Data.Aeson (ToJSON)

-- random
import System.Random (getStdRandom, random)


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
    i <- liftIO $ getStdRandom random
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
