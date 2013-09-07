{-# LANGUAGE NoImplicitPrelude
           #-}
module Game.Input ( Input (..)
                  ) where

import Summit.Prelewd

import Text.Show

import Game.Object.Type
import Physics.Types

-- | Input events understood by the game
data Input = Select Object
           | Place Position
           | Step
    deriving (Show, Eq)
