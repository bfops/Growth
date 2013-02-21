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

any' :: Foldable t => t (a -> Bool) -> a -> Bool
any' l obj = any ($ obj) l

-- | Ordering is arbitrary, but deterministic
data Object = Fire
            | Lava Bool
            | Grass
            | Water (Maybe (Bool, Bool))
            | Air
            | Rock
            | Dirt
            | Ice
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

water, rightWater, leftWater, lava, solid, cold :: Maybe Object -> Bool

water (Just (Water {})) = True
water _ = False

leftWater (Just (Water s)) = fst <$> s <?> True
leftWater _ = False

rightWater (Just (Water s)) = snd <$> s <?> True
rightWater _ = False

lava (Just (Lava _)) = True
lava _ = False

dirt, rock, ice, fire, air, grass :: Maybe Object -> Bool
[dirt, rock, ice, fire, air, grass] = Just <$> [Dirt, Rock, Ice, Fire, Air, Grass] <&> (==)

solid = any' [grass, rock, ice, dirt]

cold = any' [water, air, ice]

count :: (Maybe Object -> Bool) -> Seeds -> Integer
count p = foldr (flip $ foldr $ \x -> if' (p x) (+ 1)) 0

object :: Object -> Update
object initObj = loop (barr updateObject) ([initObj], behaviour initObj) >>> latch initObj
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
            , (cycle [Ice, Water Nothing], Water Nothing)
            ]

flagBehaviour :: Object -> Stream Id Seeds Bool -> Behaviour
flagBehaviour obj s = lift $ s >>> arr (\b -> iff b (Left obj) $ Right ())

counter :: (Maybe Object -> Bool) -> Object -> Integer -> Stream (Either Object) Seeds ()
counter p obj m = flagBehaviour obj $ updater (barr $ (+) . count p) 0 >>> arr (>= m)

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
                           else let l = rightWater $ left seeds
                                    r = leftWater $ right seeds
                                in mcond (l || r) (not l, not r)

behaviours :: Object -> [Behaviour]

behaviours Fire = [magmify, hydrophilic]
behaviours Grass = [magmify, conduct Fire, mix (Just $ Lava False) Fire]

behaviours (Water s) = [magmify, counter ice Ice 16]
                     <> mapMaybe id [s <&> \_-> flow]
    where
        flow = lift $ arr $ either (diff $ Water s) (\_-> Left Air) . (hydrophilic $<)

        diff o1 o2 = iff (o1 == o2) (Right ()) (Left o2)

behaviours (Lava b) = [wait volcano (Lava True), wait (\s -> count cold s >= iff b 2 1) Rock]
                    <> mapMaybe id [mcond b despawn]
    where
        volcano (Vector (Pair (Just (Lava _)) (Just (Lava _))) (Pair (Just (Lava _)) (Just Rock))) = True
        volcano _ = False

        despawn = wait (all $ all $ lava <&> (||) <*> rock) $ Lava False

behaviours Rock =
        [ counter water Dirt 32
        , counter fire (Lava False) 16
        , volcano
        , conduct $ Lava False
        , magmify
        ]
    where
        volcano = wait ((Just (Lava True) ==) . down) $ Lava True

behaviours Dirt = [conduct $ Lava False, magmify]

behaviours Air = [magmify, hydrophilic, counter dirt Grass 32]

behaviours Ice = [magmify, wait (any $ any warm) $ Water Nothing]
    where
        warm (Just Fire) = True
        warm obj = lava obj
