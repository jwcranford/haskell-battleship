{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.IORef (newIORef, atomicModifyIORef')
import Control.Monad.Trans (lift, liftIO)
import GHC.Generics (Generic)
import Web.Scotty (scotty, post, json)
import Data.Aeson (ToJSON)
import Battleship (Board, createValidStandardRandomBoard)
import Data.Sequence (empty, (|>), )

data Game = Game { handle :: Int, board :: Board } deriving Generic

instance ToJSON Game


main :: IO ()
main = do 
 ref <- newIORef empty
 scotty 3000 $ do
  post "/game" $ do
    b <- lift $ createValidStandardRandomBoard
    len <- liftIO $ atomicModifyIORef' ref $ \s -> (s |> b, length s)
    json $ Game len b
 
