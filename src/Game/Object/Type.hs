{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Basic game object type, and associated functions
module Game.Object.Type ( Object (..)
                        , TileUpdateIn (..)
                        , Neighbours
                        , Flow
                        -- * Board/Tiles
                        , Board
                        , Tile
                        , tileHeat
                        , tileObject
                        , makeTile
                        , heatIn
                        , objectIn
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
                        , basalt
                        -- * Neighbour accessors
                        , left
                        , right
                        , up
                        , down
                        -- * Heat and temperature info
                        , capacity
                        , resistence
                        , initHeat
                        , initTemp
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
            | Basalt
    deriving (Show, Read, Eq, Ord, Generic)

instance Hashable Object

-- | A `Vector` holding a value from each of a tile's neighbors
-- (one in each direction, i.e. two in each dimension).
-- The value may be @Nothing@ if the tile is on a world edge
-- (i.e. has no neighbors on that side).
type Neighbours v = Vector (Pair (Maybe v))

data Tile = Tile
    { _tileObject :: Object
    , _tileHeat :: Heat
    }
  deriving (Show, Read, Eq, Ord, Generic)

$(makeLenses ''Tile)

type Board = Array Position

makeTile :: Object -> Tile
makeTile obj = Tile obj (initHeat obj)

-- | Update information for a single `Tile`.
data TileUpdateIn = TileUpdateIn
    { tileIn :: Tile
    , neighbours :: Neighbours Tile
    }
  deriving (Show, Read, Eq)

heatIn :: TileUpdateIn -> Heat
heatIn = view tileHeat . tileIn

objectIn :: TileUpdateIn -> Object
objectIn = view tileObject . tileIn

-- | Given a Vector of "neighbours", get a specific one.
left, right, down, up :: TileUpdateIn -> Maybe Tile
[left, right, down, up] = getElem <$> toList dimensions <*> (pair <$> [\x _-> x, \_ x -> x])
  where
    getElem dim f = f . view (component dim) . neighbours

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

fire, air, dirt, rock, ice, grass, snow, basalt :: Maybe Tile -> Bool
[fire, air, dirt, rock, ice, grass, snow, basalt]
    = [Fire, Air, Dirt, Rock, Ice, Grass, Snow, Basalt]
  <&> \o1 o2 -> Just o1 == (view tileObject <$> o2)

solid = any' [grass, rock, ice, dirt, snow, basalt]

initTemp :: Object -> Temperature
initTemp o = realToFrac (initHeat o) / realToFrac (capacity o)

initHeat :: Object -> Heat
initHeat Fire = 4
initHeat (Lava _) = 128
initHeat Ice = -16
initHeat Snow = -1
initHeat Basalt = 0
initHeat Grass = 0
initHeat (Water _) = 0
initHeat Air = 0
initHeat Rock = 0
initHeat Dirt = 0

-- | The ratio between how much energy an object takes on and how much its
-- temperature changes. When thinking about the temperature equilibrium
-- between two objects, a higher heat capacity moves the point of equilibrium
-- closer to that object.
capacity :: Object -> HeatCapacity
capacity Fire = 1/64
capacity (Lava _) = 1/64
capacity Grass = 1
capacity (Water _) = 32
capacity Air = 1
capacity Rock = 1
capacity Dirt = 1
capacity Ice = 32
capacity Snow = 32
capacity Basalt = 32

-- | An object's resistence to taking on energy. When looking at the
-- temperature equilibrium between two objects, a higher resistence product
-- means the objects will take longer to reach equilibrium. Similarly, a
-- higher resistence also means an Object will take longer to conduct heat.
resistence :: Object -> HeatResistence
resistence Fire = 1
resistence (Lava _) = 1
resistence Grass = 1
resistence (Water _) = 1
resistence Air = 1 + floor (64 * capacity Air)
resistence Rock = 1 + floor (64 * capacity Rock)
resistence Dirt = 1 + floor (64 * capacity Dirt)
resistence Ice = 1
resistence Snow = 1
resistence Basalt = 1 + floor (capacity Basalt / 16)
