{-# LANGUAGE DeriveGeneric #-}
-- | Basic game object type, and associated functions
module Game.Object.Type ( Object (..)
                        , Flow
                        , Seeds
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
                        , left
                        , right
                        , up
                        , down
                        , solid
                        ) where

import Control.Applicative
import Control.Lens
import Data.Hashable
import Data.Foldable as Foldable
import Data.Pair
import GHC.Generics

import Game.Vector

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

-- | Spawns from a set of neighbours
type Seeds = Vector (Pair (Maybe Object))

-- | Given a Vector of "neighbours", get a specific one.
left, right, down, up :: Vector (Pair a) -> a
[left, right, down, up] = getElem <$> toList dimensions <*> (pair <$> [\x _-> x, \_ x -> x])
  where
    getElem dim f = f . view (component dim)

water, rightWater, leftWater, lava, solid :: Maybe Object -> Bool

water (Just (Water {})) = True
water _ = False

leftWater (Just (Water s)) = maybe True fst s
leftWater _ = False

rightWater (Just (Water s)) = maybe True snd s
rightWater _ = False

lava (Just (Lava _)) = True
lava _ = False

fire, air, dirt, rock, ice, grass, snow :: Maybe Object -> Bool
[fire, air, dirt, rock, ice, grass, snow] = Just <$> [Fire, Air, Dirt, Rock, Ice, Grass, Snow] <&> (==)

solid = any' [grass, rock, ice, dirt, snow]
