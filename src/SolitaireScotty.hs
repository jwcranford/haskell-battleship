{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.IORef (newIORef, atomicModifyIORef')
import Control.Monad.Trans (lift, liftIO)
import GHC.Generics (Generic)
import Web.Scotty (scotty, post, json)
import Data.Aeson (ToJSON)
import Battleship (Board, createValidStandardRandomBoard)
-- import Data.Map

data Game = Game { handle :: Int, board :: Board } deriving Generic

instance ToJSON Game


main :: IO ()
main = do 
 ref <- newIORef 0
 scotty 3000 $ do
  post "/game" $ do
    num <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1,i+1)
    b <- lift $ createValidStandardRandomBoard
    json $ Game num b
 
