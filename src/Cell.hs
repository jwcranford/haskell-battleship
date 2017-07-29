{-# LANGUAGE DeriveGeneric #-}

module Cell where

import Control.Monad
import Ship
import GHC.Generics
import Data.Aeson (ToJSON)

data Cell = Vacant (Maybe Bool) 
        | Occupied Ship Bool 
        | Collision Ship Ship
        deriving (Eq, Generic)

instance Show Cell where
    show (Vacant Nothing)      = "."
    show (Vacant (Just True))  = "x"
    show (Vacant (Just False)) = "*"
    show (Occupied _ True)     = "X"
    show (Occupied _ False)    = "O"
    show (Collision _ _)       = "#"

-- Cells can be combined as long as at least one is Vacant
instance Monoid Cell where
    mempty = Vacant Nothing

    mappend (Vacant ma) (Vacant mb) =
        Vacant $ liftM2 (\a b -> or [a, b]) ma mb
    mappend (Vacant _) occ@(Occupied _ _) = occ
    mappend occ@(Occupied _ _) (Vacant _) = occ
    mappend (Occupied s1 _) (Occupied s2 _) = Collision s1 s2
    mappend c@(Collision _ _) _ = c
    mappend _ c@(Collision _ _) = c

instance ToJSON Cell

-- simple predicate for whether a cell is a collision or not
collision :: Cell -> Bool
collision (Collision _ _) = True
collision _ = False
    
