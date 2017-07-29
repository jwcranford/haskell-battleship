{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.IORef
import Control.Monad.Trans
import GHC.Generics
import Web.Scotty
import Data.Aeson (ToJSON)
import Battleship


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
 
