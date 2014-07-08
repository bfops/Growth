{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Basic game object type, and associated functions
module Game.Object.Type ( Object (..)
                        , Neighbours
                        , Flow
                        -- * Board/Tiles
                        , Board
                        , Tile
                        , makeTile
                        , tileObject
                        -- * Object comparators
                        , fire
                        , lava
                        , grass
                        , water
                        , leftWater
                        , rightWater
                        , air
                        , rock
                        , dirt
                        , ice
                        , snow
                        , solid
                        -- * Neighbour accessors
                        , left
                        , right
                        , up
                        , down
                        ) where

import Control.Applicative
import Control.Lens
import Data.Array.IArray
import Data.Hashable
import Data.Foldable as Foldable
import Data.Pair
import GHC.Generics

import Game.Vector
import Physics.Types

any' :: Foldable t => t (a -> Bool) -> a -> Bool
any' l obj = Foldable.any ($ obj) l

type Flow = Maybe (Bool, Bool)

-- | Ordering is arbitrary, but deterministic
data Object = Fire
            | Lava Bool
            | Grass
            | Water Flow
            | Air
            | Rock
            | Dirt
            | Ice
            | Snow
    deriving (Show, Read, Eq, Ord, Generic)

instance Hashable Object

-- | A `Vector` holding a value from each of a tile's neighbors
-- (one in each direction, i.e. two in each dimension).
-- The value may be @Nothing@ if the tile is on a world edge
-- (i.e. has no neighbors on that side).
type Neighbours v = Vector (Pair (Maybe v))

data Tile = Tile
    { _tileObject :: Object
    }
  deriving (Show, Read, Eq, Ord, Generic)

$(makeLenses ''Tile)

type Board = Array Position

makeTile :: Object -> Tile
makeTile obj = Tile obj

-- | Given a Vector of "neighbours", get a specific one.
left, right, down, up :: Vector (Pair a) -> a
[left, right, down, up] = getElem <$> toList dimensions <*> (pair <$> [\x _-> x, \_ x -> x])
  where
    getElem dim f = f . view (component dim)

water, rightWater, leftWater, lava, solid :: Maybe Tile -> Bool

water t =
  case view tileObject <$> t of
    Just (Water {}) -> True
    _ -> False

leftWater t =
  case view tileObject <$> t of
    Just (Water s) -> maybe True fst s
    _ -> False

rightWater t =
  case view tileObject <$> t of
    Just (Water s) -> maybe True snd s
    _ -> False
lava t =
  case view tileObject <$> t of
    Just (Lava _) -> True
    _ -> False

fire, air, dirt, rock, ice, grass, snow :: Maybe Tile -> Bool
[fire, air, dirt, rock, ice, grass, snow]
    = [Fire, Air, Dirt, Rock, Ice, Grass, Snow]
  <&> \o1 o2 -> Just o1 == (view tileObject <$> o2)

solid = any' [grass, rock, ice, dirt, snow]
