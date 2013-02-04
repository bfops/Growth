{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
-- | Basic game object type, and associated functions
module Game.Object ( Object (..)
                   , Seeds
                   , Update
                   , object
                   , mix
                   ) where

import Prelewd

import Control.Stream
import Data.Tuple
import Storage.Id
import Storage.Member
import Storage.Pair
import Text.Show

import Game.Vector

data Object = Fire
            | Lava Bool
            | Grass
            | Water Bool
            | Air
            | Rock
            | Dirt
    deriving (Show, Eq)

-- | Spawns from a set of neighbours
type Seeds = Vector (Pair (Maybe Object))
type Update = Stream Id Seeds Object
type Behaviour = Stream (Either Object) Seeds ()

-- | What Object is in which neighbour?
left, right, down, up :: Seeds -> Maybe Object
(Vector (Pair left right) (Pair down up)) = getObj <$> dimensions <%> Pair fst snd
    where
        getObj dim f = f . pair (,) . component dim

water, lava, dirt, transparent, solid :: Maybe Object -> Bool

water (Just (Water _)) = True
water _ = False

dirt = (== Just Dirt)

lava (Just (Lava _)) = True
lava _ = False

transparent = (`elem` lst)
    where
        lst = Just <$> [Water False, Water True, Fire, Air]

solid = (`elem` lst)
    where
        lst = Just <$> [Grass, Rock, Dirt]

mix :: Maybe Object -> Object -> Behaviour
mix obj result = wait (any $ any (== obj)) result

conduct :: Object -> Behaviour
conduct = mix =<< Just

magmify, hydrophilic :: Behaviour

magmify = mix (Just $ Lava True) $ Lava False

hydrophilic = sequence_ [wait (water . up) $ Water False, wait flow $ Water True]
    where
        sideWater = (== Just (Water True))
        flow = sideWater . left <&> (||) <*> sideWater . right

count :: (Maybe Object -> Bool) -> Seeds -> Integer
count p = foldr (flip $ foldr $ \x -> if' (p x) (+ 1)) 0

object :: Object -> Update
object initObj = blackBox updateObject ([initObj], behaviour initObj) >>> latch initObj
    where
        behaviour obj = sequence_ $ behaviours obj
        updateObject seeds (hist, s) = case s $< seeds of
                    Left obj -> if elem obj hist
                                then (Just obj, ([obj], behaviour obj))
                                else case updateObject seeds (obj:hist, behaviour obj) of
                                    (Nothing, _) -> (Just obj, ([obj], behaviour obj))
                                    x -> x
                    Right (_, s') -> (Nothing, (hist, s'))

flagBehaviour :: Object -> Stream Id Seeds Bool -> Behaviour
flagBehaviour obj s = lift $ s >>> arr (\b -> iff b (Left obj) $ Right ())

counter :: (Maybe Object -> Bool) -> Object -> Integer -> Stream (Either Object) Seeds ()
counter p obj m = flagBehaviour obj $ arr Just >>> updater ((+) . count p) 0 >>> arr (>= m)

wait :: (Seeds -> Bool) -> Object -> Behaviour
wait p obj = flagBehaviour obj $ arr p

behaviours :: Object -> [Behaviour]

behaviours Fire = [magmify, hydrophilic]
behaviours Grass = [magmify, conduct Fire, mix (Just $ Lava False) Fire]

behaviours (Water b) = let switch = wait (iff b transparent solid . down) $ Water $ not b
                         in [magmify, mix (Just $ Lava False) Air, switch]

behaviours (Lava b) = [wait volcano (Lava True), iff b despawn $ mix (Just Air) Rock]
    where
        volcano (Vector (Pair (Just (Lava _)) (Just (Lava _))) (Pair (Just (Lava _)) (Just Rock))) = True
        volcano _ = False

        despawn = wait (all $ all lava) $ Lava False

behaviours Rock =
        [ counter water Dirt 32
        , counter (== Just Fire) (Lava False) 16
        , volcano
        , smelt
        , magmify
        ]
    where
        volcano = wait ((Just (Lava True) ==) . down <&> (&&) <*> molten . left <&> (&&) <*> molten . right) $ Lava True

        smelt = wait (any (any $ (== Just (Lava False))) <&> (&&) <*> all (all $ (/= Just Air))) $ Lava False

        molten (Just Rock) = True
        molten (Just (Lava _)) = True
        molten _ = False

behaviours Dirt = [conduct $ Lava False, magmify]

behaviours Air = [magmify, hydrophilic, counter dirt Grass 32]
