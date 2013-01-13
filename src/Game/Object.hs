{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Basic game object type, and associated functions
module Game.Object ( Object (..)
                   ) where

import Prelewd

import Text.Show

data Object = Fire
            | Grass
            | Water
    deriving (Show, Eq, Ord)
