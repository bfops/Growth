{-# LANGUAGE NoImplicitPrelude
           #-}
module Game.Input ( Input (..)
                  ) where

import Prelewd

import Text.Show

import Game.Object
import Physics.Types

-- | Input events understood by the game
data Input = Select Object
           | Place Position
           | Step
    deriving (Show, Eq)
