{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Input handling
module Game.Input ( Input (..)
                  , Inputs
                  , ButtonState
                  ) where

import Prelewd

import Text.Show

import Wrappers.Events

import Game.Object
import Physics.Types

-- | Input events understood by the game
data Input = Select Object
           | Place Position
    deriving (Show, Eq, Ord)

type Inputs = [Input]
