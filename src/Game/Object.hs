{-# LANGUAGE NoImplicitPrelude
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

import Storage.Pair
import Text.Show

import Game.Vector
import Util.Id
import Util.Stream

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
type Update = Stream Id (Maybe Seeds) (Object, Spawn)

all, gravity :: Spawn
all = pure $ pure True
gravity = component' Height (\_-> Pair True False) all

-- | What does an Object produce in neighbouring cells?
spawn :: Object -> Spawn
spawn Fire = all
spawn Lava = all
spawn Grass = all
spawn Water = gravity
spawn Air = all
spawn Rock = all

-- | `mix a b` mixes a and b to produce a new b
mix :: Object -> Object -> Object

mix Water Lava = Rock
mix _ Lava = Lava

mix Lava Rock = Lava
mix _ Rock = Rock

mix Air x = x
mix Rock x = x

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

mixV :: Vector (Pair (Maybe Object)) -> Object -> Object
mixV (Vector (Pair left right) (Pair up down)) obj = foldl (flip mix) obj $ mapMaybe id [up, left, right, down]

object :: Update
object = updater (\v (o, _) -> ((,) <*> spawn) $ mixV v o) (Air, all)
