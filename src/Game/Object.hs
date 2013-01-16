{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Basic game object type, and associated functions
module Game.Object ( Object (..)
                   , spawn
                   , mix
                   ) where

import Prelewd

import Impure

import Storage.Map
import Storage.Pair
import Text.Show

data Object = Fire
            | Grass
            | Water
            | Air
            | Rock
    deriving (Show, Eq, Ord)

-- | What does an Object produce in neighbouring cells?
spawn :: Object -> Object
spawn Fire = Fire
spawn Water = Water
spawn Grass = Grass
spawn Air = Air
spawn Rock = Air

-- | Combine two overlapping Objects two produce a new one.
-- `mix` is commutative, but not associative.
mix :: Object -> Object -> Object
mix Air x = x
mix x Air = x
mix x y = if (x == y)
          then x
          else lookup (Pair x y) mixes
           <?> error ("No combination for mixing " <> show x <> " with " <> show y)

mixes :: Map (Pair Object) Object
mixes = fromList $
      [ (Pair Fire Grass    , Fire)
      , (Pair Grass Water   , Grass)
      , (Pair Fire Water    , Water)
      , (Pair Rock Water    , Rock)
      , (Pair Rock Fire     , Rock)
      , (Pair Rock Grass    , Rock)
      ]
