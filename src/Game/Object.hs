{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
-- | Basic game object type, and associated functions
module Game.Object ( Object (..)
                   , Seeds
                   , Update
                   , object
                   , Spawn
                   , spawn
                   , mix
                   ) where

import Prelewd hiding (all)

import Control.Stream
import Storage.Id
import Storage.Pair
import Text.Show

import Game.Vector

data Object = Fire
            | Lava
            | Grass
            | Water
            | Air
            | Rock
    deriving (Show, Eq, Ord)

-- | The directions in which to spawn
type Spawn = Vector (Pair Bool)
-- | Spawns from a set of neighbours
type Seeds = Vector (Pair (Maybe Object))
type Update = Stream Id Seeds (Object, Spawn)

all, none, gravity :: Spawn
all = pure $ pure True
none = pure $ pure False
gravity = component' Height (\_-> Pair True False) all

-- | What does an Object produce in neighbouring cells?
spawn :: Object -> Stream Id Seeds Spawn
spawn Fire = pure all
spawn Lava = pure all
spawn Grass = pure all
spawn Water = pure gravity
spawn Air = pure all
spawn Rock = pure none

-- | `mix a b` mixes a and b to produce a new b
mix :: Object -> Object -> Object

mix Air Lava = Rock
mix _ Lava = Lava

mix Lava Rock = Lava
mix _ Rock = Rock

mix Air x = x
mix Rock _ = Rock

mix Fire Fire = Fire
mix Fire Grass = Fire
mix Fire Water = Water
mix Fire Air = Air

mix Lava Fire = Fire
mix Lava Grass = Fire
mix Lava Water = Lava
mix Lava Air = Air

mix Water Fire = Air
mix Water Grass = Grass
mix Water Water = Water
mix Water Air = Water

mix Grass Fire = Fire
mix Grass Grass = Grass
mix Grass Water = Grass
mix Grass Air = Air

object :: Update
object = (arr Just >>> updater mixNeighbours Air) &&& id
       >>> arr (\(x, y) -> (x, (x, y)))
       >>> map (blackBox advanceSpawn (Air, spawn Air))
    where
        advanceSpawn (o2, seeds) (o1, s) = (o2,) <$> runId (iff (o1 == o2) s (spawn o2) $< seeds)

mixNeighbours :: Vector (Pair (Maybe Object)) -> Object -> Object
mixNeighbours v obj = mixV v
    where
        mixV (Vector (Pair left right) (Pair down up)) = foldl (flip mix) obj $ mapMaybe id [up, left, right, down]
