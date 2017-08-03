{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.IORef (newIORef, atomicModifyIORef', readIORef)
import Control.Monad.Trans (lift, liftIO)
import GHC.Generics (Generic)
import Web.Scotty (scotty, post, get, param, json, status)
import Network.HTTP.Types.Status (notFound404)
import Data.Aeson (ToJSON)
import Battleship (Board, createValidStandardRandomBoard)
import Data.Sequence (empty, (|>), (!?))

data Game = Game { handle :: Int, board :: Board } deriving Generic

instance ToJSON Game


main :: IO ()
main = do 
 ref <- newIORef empty
 scotty 3000 $ do
  post "/game" $ do
    b <- lift $ createValidStandardRandomBoard
    gs <- liftIO $ readIORef ref
    let g = Game (length gs) b
    liftIO $ atomicModifyIORef' ref $ \s -> (s |> g, ())
    json $ g
  get "/game/:id" $ do
    i <- param "id"
    gs <- liftIO $ readIORef ref
    let mgame = gs !? i
    case mgame of
      Just game -> json game
      Nothing   -> status notFound404
    
