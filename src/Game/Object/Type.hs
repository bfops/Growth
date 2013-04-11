{-# LANGUAGE NoImplicitPrelude
           #-}
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
                        ) where

import Prelewd

import Data.Tuple
import Storage.Pair
import Text.Show

import Game.Vector

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
    deriving (Show, Eq, Ord)

-- | A set of spawns from the neighbours
type Seeds = Vector (Pair (Maybe Object))

-- | What Object is in which neighbour?
left, right, down, up :: Seeds -> Maybe Object
(Vector (Pair left right) (Pair down up)) = getObj <$> dimensions <%> Pair fst snd
    where
        getObj dim f = f . pair (,) . component dim

water, rightWater, leftWater, lava :: Maybe Object -> Bool

water (Just (Water {})) = True
water _ = False

leftWater (Just (Water s)) = fst <$> s <?> True
leftWater _ = False

rightWater (Just (Water s)) = snd <$> s <?> True
rightWater _ = False

lava (Just (Lava _)) = True
lava _ = False

fire, air, dirt, rock, ice, grass, snow :: Maybe Object -> Bool
[fire, air, dirt, rock, ice, grass, snow] = Just <$> [Fire, Air, Dirt, Rock, Ice, Grass, Snow] <&> (==)
