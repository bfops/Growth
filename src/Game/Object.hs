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
import Storage.Id
import Storage.List
import Storage.Pair
import Text.Show

import Game.Vector
import Physics.Types

any' :: Foldable t => t (a -> Bool) -> a -> Bool
any' l obj = any ($ obj) l

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
    deriving (Show, Eq, Ord)

-- | Spawns from a set of neighbours
type Seeds = Vector (Pair (Maybe Object))
type Update = Stream Id Seeds Object
type Behaviour = Stream Maybe Seeds ()
type Transformation = Stream (Either Object) Seeds ()
type Board = Array Position Object

infix 1 =>>
(=>>) :: Behaviour -> Object -> Transformation
b =>> obj = Stream $ \seeds -> (=>> obj) <$$> (b $< seeds) <&> Right <?> Left obj

-- | What Object is in which neighbour?
left, right, down, up :: Seeds -> Maybe Object
(Vector (Pair left right) (Pair down up)) = getObj <$> dimensions <%> Pair fst snd
    where
        getObj dim f = f . pair (,) . component dim

water, rightWater, leftWater, lava, solid :: Maybe Object -> Bool

water (Just (Water {})) = True
water _ = False

leftWater (Just (Water s)) = fst <$> s <?> True
leftWater _ = False

rightWater (Just (Water s)) = snd <$> s <?> True
rightWater _ = False

lava (Just (Lava _)) = True
lava _ = False

dirt, rock, ice, grass :: Maybe Object -> Bool
[dirt, rock, ice, grass] = Just <$> [Dirt, Rock, Ice, Grass] <&> (==)

solid = any' [grass, rock, ice, dirt]

object :: Object -> Update
object initObj = loop (barr updateObject) ([initObj], behaviour initObj) >>> latch initObj
    where
        accum obj = (Just obj, ([obj], behaviour obj))
        behaviour obj = sequence_ $ transformations obj
        updateObject seeds (hist, s) = either accum ((Nothing,) . (hist,) . snd) (s $< seeds)

wait :: Stream Id Seeds Bool -> Behaviour
wait s = lift $ s >>> arr (\b -> mcond (not b) ())

infixl 2 `except`
-- | Create a Behaviour which, upon termination, will first check some other Behaviour for confirmation.
-- Note that both Behaviours advance every time the resultant Behaviour advances.
except :: Behaviour     -- ^ Terminating Behaviour
       -> Behaviour     -- ^ Upon termination, check this Behaviour.
                        -- If it also terminates, refute the termination of the original Behaviour.
       -> Behaviour
b `except` ex = Stream $ \seeds -> case (b $< seeds, ex $< seeds) of
                            (Nothing, Just _) -> Nothing
                            (b', ex') -> Just ((), snd <$> b' <?> b `except` snd <$> ex' <?> ex)

count :: Integer -> (Maybe Object -> Bool) -> Behaviour
count n p = wait $ updater (barr $ (+) . countPs) 0 >>> arr (>= n)
    where
        countPs = foldr (flip $ foldr $ \x -> if' (p x) (+ 1)) 0

mix :: Object -> Behaviour
mix = wait . arr . any . any . (==) . Just

waterFlow :: Stream Id Seeds (Maybe Flow)
waterFlow = arr $ \seeds -> considerGround (down seeds) <$> resultFlow seeds
    where
        considerGround ground flow = Just $ iff (solid ground) flow (False, False)

        resultFlow seeds = if water $ up seeds
                           then Just (True, True)
                           else let l = rightWater $ left seeds
                                    r = leftWater $ right seeds
                                in mcond (l || r) (not l, not r)

-- | Incorporate flowing Water
hydrophilic :: Behaviour
hydrophilic = lift $ waterFlow >>> arr (\m -> (\_-> Nothing) <$> m <?> Just ())

-- | Water will flow through these tiles
waterThrough :: Transformation
waterThrough = lift $ waterFlow >>> arr (\m -> Left . Water <$> m <?> Right ())

magmify :: Transformation
magmify = mix (Lava True) =>> Lava False

heat :: Integer -> Behaviour
heat 0 = error $ "`heat 0` causes instant change"
heat n = wait
       $ arr flatten
     >>> several (updater (barr $ try $ (+) . deltaHeat) 0)
     >>> arr (\l -> iff (n > 0) (>= n) (<= n) $ last l <?> 0)
    where
        flatten seeds = toList seeds >>= toList

        deltaHeat Fire = 2
        deltaHeat (Lava b) = iff b 12 8
        deltaHeat Ice = -1
        deltaHeat _ = 0

lavaToRock :: Behaviour
lavaToRock = sequence_ [hydrophilic, mix Air]

transformations :: Object -> [Transformation]

transformations Fire = [magmify, waterThrough]
transformations Grass = [magmify, mix Fire =>> Fire, mix (Lava False) =>> Fire]

transformations (Water s) = [magmify, heat (-16) =>> Ice]
                         <> mapMaybe id [s <&> \_-> flow]
    where
        flow = lift $ arr $ either (diff $ Water s) (\_-> Left Air) . (waterThrough $<)

        diff o1 o2 = iff (o1 == o2) (Right ()) (Left o2)

transformations (Lava b) = [wait (arr volcano) =>> Lava True, lavaToRock =>> Rock]
                        <> mcond b despawn
    where
        volcano seeds = all lava ([left, right, down] <&> ($ seeds)) && (rock (up seeds) || dirt (up seeds))

        despawn = wait (arr $ all $ all $ lava <&> (||) <*> rock) =>> Lava False

transformations Rock =
        [ count 32 water `except` wait (arr $ any $ any lava) =>> Dirt
        , volcano
        , heat 8 `except` lavaToRock =>> Lava False
        , magmify
        ]
    where
        volcano = wait (arr $ (Just (Lava True) ==) . down) =>> Lava True

transformations Dirt = [mix (Lava False) =>> Lava False, magmify]

transformations Air = [magmify, waterThrough, count 32 dirt =>> Grass]

transformations Ice = [magmify, heat 2 =>> Water Nothing]
