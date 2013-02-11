{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
-- | Basic game object type, and associated functions
module Game.Object ( Object (..)
                   , Board
                   , Seeds
                   , Update
                   , object
                   , mix
                   ) where

import Prelewd

import Impure

import Control.Stream
import Data.Tuple
import Storage.Array
import Storage.Cycle
import Storage.Id
import Storage.List hiding (cycle)
import Storage.Map
import Storage.Member
import Storage.Pair
import Text.Show

import Game.Vector
import Physics.Types

-- | Ordering is arbitrary, but deterministic
data Object = Fire
            | Lava Bool
            | Grass
            | Water (Maybe (Bool, Bool))
            | Air
            | Rock
            | Dirt
    deriving (Show, Eq, Ord)

-- | Spawns from a set of neighbours
type Seeds = Vector (Pair (Maybe Object))
type Update = Stream Id Seeds Object
type Behaviour = Stream (Either Object) Seeds ()
type Board = Array Position Object

-- | What Object is in which neighbour?
left, right, down, up :: Seeds -> Maybe Object
(Vector (Pair left right) (Pair down up)) = getObj <$> dimensions <%> Pair fst snd
    where
        getObj dim f = f . pair (,) . component dim

water, lava, dirt, solid :: Maybe Object -> Bool

water (Just (Water {})) = True
water _ = False

dirt = (== Just Dirt)

lava (Just (Lava _)) = True
lava _ = False

solid = (`elem` lst)
    where
        lst = Just <$> [Grass, Rock, Dirt]

count :: (Maybe Object -> Bool) -> Seeds -> Integer
count p = foldr (flip $ foldr $ \x -> if' (p x) (+ 1)) 0

object :: Object -> Update
object initObj = blackBox updateObject ([initObj], behaviour initObj) >>> latch initObj
    where
        accum obj = (Just obj, ([obj], behaviour obj))
        behaviour obj = sequence_ $ behaviours obj
        updateObject seeds (hist, s) = case s $< seeds of
                    Left obj -> if elem obj hist
                                then accum $ resolveCycle $ reverse $ obj : takeWhile (/= obj) hist
                                else case updateObject seeds (obj:hist, behaviour obj) of
                                    (Nothing, _) -> accum obj
                                    x -> x
                    Right (_, s') -> (Nothing, (hist, s'))

resolveCycle :: [Object] -> Object
resolveCycle [obj] = obj
resolveCycle c = lookup (cycle c) cycles <?> error ("No cycle resolution for " <> show c)
    where
        cycles = fromList $
            [ (cycle [Rock, Lava False], Rock)
            , (cycle [Rock, Lava True], Rock)
            , (cycle [Lava False, Lava True], Lava True)
            ]

flagBehaviour :: Object -> Stream Id Seeds Bool -> Behaviour
flagBehaviour obj s = lift $ s >>> arr (\b -> iff b (Left obj) $ Right ())

counter :: (Maybe Object -> Bool) -> Object -> Integer -> Stream (Either Object) Seeds ()
counter p obj m = flagBehaviour obj $ arr Just >>> updater (Id <$$> (+) . count p) 0 >>> arr (>= m)

wait :: (Seeds -> Bool) -> Object -> Behaviour
wait p obj = flagBehaviour obj $ arr p

mix :: Maybe Object -> Object -> Behaviour
mix obj result = wait (any $ any (== obj)) result

conduct :: Object -> Behaviour
conduct = mix =<< Just

magmify, hydrophilic :: Behaviour

magmify = mix (Just $ Lava True) $ Lava False

hydrophilic = lift $ arr $ \seeds -> constructWater (down seeds) <$> resultFlow seeds <?> Right ()
    where
        constructWater ground flow = Left $ Water $ Just $ iff (solid ground) flow (False, False)

        resultFlow seeds = if water $ up seeds
                           then Just (True, True)
                           else let l = flowsRight $ left seeds
                                    r = flowsLeft $ right seeds
                                in mcond (l || r) (not l, not r)

        flowsLeft (Just (Water s)) = fst <$> s <?> True
        flowsLeft _ = False

        flowsRight (Just (Water s)) = snd <$> s <?> True
        flowsRight _ = False

behaviours :: Object -> [Behaviour]

behaviours Fire = [magmify, hydrophilic]
behaviours Grass = [magmify, conduct Fire, mix (Just $ Lava False) Fire]

behaviours (Water s) = [magmify] <> mapMaybe id [s <&> \_-> flow]
    where
        flow = lift $ arr $ either (diff $ Water s) (\_-> Left Air) . (hydrophilic $<)

        diff o1 o2 = iff (o1 == o2) (Right ()) (Left o2)

behaviours (Lava b) = [wait volcano (Lava True), wait (\s -> count cold s >= iff b 2 1) Rock]
                    <> mapMaybe id [mcond b despawn]
    where
        volcano (Vector (Pair (Just (Lava _)) (Just (Lava _))) (Pair (Just (Lava _)) (Just Rock))) = True
        volcano _ = False

        cold (Just Air) = True
        cold obj = water obj

        despawn = wait (all $ all $ lava <&> (||) <*> (== Just Rock)) $ Lava False

behaviours Rock =
        [ counter water Dirt 32
        , counter (== Just Fire) (Lava False) 16
        , volcano
        , conduct $ Lava False
        , magmify
        ]
    where
        volcano = wait ((Just (Lava True) ==) . down) $ Lava True

behaviours Dirt = [conduct $ Lava False, magmify]

behaviours Air = [magmify, hydrophilic, counter dirt Grass 32]
