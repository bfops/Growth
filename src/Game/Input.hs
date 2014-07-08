{-# LANGUAGE DeriveDataTypeable #-}
module Game.Input ( Input (..)
                  , Tick (..)
                  ) where

import Data.Typeable
import Game.Object.Type
import Physics.Types

-- | Input events understood by the game
data Input = Select Object -- ^ Select an object type
           | Place Position -- ^ Place an object
           | Step -- ^ Advance the world state
  deriving (Show, Eq, Typeable)

data Tick = Tick -- ^ Just a heartbeat/time increment
  deriving (Show, Eq, Typeable)
