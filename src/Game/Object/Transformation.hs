{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
module Game.Object.Transformation ( Transformation
                                  , NewState (..)
                                  , transformations
                                  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Conduit as Conduit
import Data.Conduit.List as Conduit
import Data.Functor
import Data.Foldable as Foldable
import Data.Monoid

import Data.Conduit.Extra as Conduit
import Game.Object.Type

data NewState
    = State Object Bool
  deriving (Show, Read, Eq, Ord)

state :: Object -> NewState
state o = State o False

rerun :: Object -> NewState
rerun o = State o True

type Transformation m = Sink (Neighbours Tile) m NewState

-- | Wait for a value to surpass a target (in either direction)
exceed :: (Num i, Ord i, Monad m) => i -> Sink i m ()
exceed target = awaitUntil (\i -> signum i == signum target && abs i > abs target) ()

-- | wait until the output becomes True.
wait :: Monad m => (i -> Bool) -> Sink i m ()
wait s = Conduit.map s =$= awaitUntil id ()

-- | Accumulate a count.
count :: Monad m => (i -> Integer) -> Conduit i m Integer
count c = Conduit.map c =$= void (scan (+) 0)

mapreduce :: (Functor t, Foldable t) => (b -> b -> b) -> b -> (a -> b) -> t a -> b
mapreduce reduce b f = Foldable.foldl' reduce b . fmap f

mapsum :: (Functor t, Foldable t, Num b) => (a -> b) -> t a -> b
mapsum = mapreduce (+) 0

neighbour :: (Maybe Tile -> Bool) -> Neighbours Tile -> Integer
neighbour n = mapsum $ mapsum $ \x -> if n x then 1 else 0

mix :: Object -> Neighbours Tile -> Bool
mix obj = Foldable.any $ Foldable.any $ maybe False ((== obj) . view tileObject)

-- | Keep track of accumulated heat from neighbors.
heat :: Monad m => Conduit (Neighbours Tile) m Integer
heat = count $ mapsum $ mapsum $ maybe 0 (deltaHeat . view tileObject)
  where
    deltaHeat Fire = 2
    deltaHeat (Lava True) = 12
    deltaHeat (Lava False) = 8
    deltaHeat Ice = -1
    deltaHeat _ = 0

-- | Get info about the water flowing through this tile from info about the neighbors.
waterFlow :: Neighbours Tile -> Maybe Flow
waterFlow s
    = let
        footing = down s
        above = up s
        l = rightWater $ left s
        r = leftWater $ right s
      in do
        guard $ water above || l || r
        Just $ Just $
          if not (solid footing)
          then (False, False)
          else
            if water above
            then (True, True)
            else (not l, not r)

-- | Incorporate flowing Water
hydrophilic :: Neighbours Tile -> Bool
hydrophilic = maybe False (\_-> True) . waterFlow

-- | Water will flow through (and occupy) these tiles
acceptWater :: Monad m => Transformation m
acceptWater = do
    neighbors <- waterFlow <$> awaitJust
    case neighbors of
      Nothing -> acceptWater
      Just w -> return $ state $ Water w

magmify :: Monad m => Transformation m
magmify = wait (mix $ Lava True) $> state (Lava False)

-- | The conditions for Lava to convert to Rock
lavaToRock :: Neighbours Tile -> Bool
lavaToRock s = Foldable.any ($ s) [hydrophilic, mix Air]

snowFall :: Monad m => Transformation m
snowFall = wait (snow . up) $> state Snow

transformations :: Monad m => Object -> [Transformation m]

transformations Fire = [magmify, acceptWater]
transformations Grass =
    [ magmify
    , heat =$ exceed 2 $> state Fire
    ]

transformations (Water s)
    = let flowOrNull = maybe [] (\_-> [flow]) s
      in [magmify, heat =$ exceed (-16) $> state Ice] <> flowOrNull <> [snowFall]
  where
    flow = do
      f <- waterFlow <$> awaitJust
      case f of
        -- This is a non-source block and no more water is flowing through this tile.
        Nothing -> return $ state Air
        Just s' ->
          if s == s'
          then flow
          else return $ state (Water s')

transformations (Lava b)
    = [wait volcano $> state (Lava True), wait lavaToRock $> state Rock]
    <> (if b then [despawn] else [])
  where
      volcano s
          = Foldable.all lava ([left, right, down] <&> ($ s))
          && (rock (up s) || dirt (up s))

      despawn = wait (Foldable.all (Foldable.all molten)) $> state (Lava False)

      molten obj = lava obj || rock obj

transformations Rock =
        [ count (neighbour water) =$ exceed 32 $> rerun Dirt
        , volcano
        , heat =$ exceed 8 $> rerun (Lava False)
        , magmify
        ]
    where
        volcano = wait (maybe False ((Lava True ==) . view tileObject) . down) $> state (Lava True)

transformations Dirt = [wait (mix $ Lava False) $> state (Lava False), magmify]

transformations Air = [magmify, acceptWater, count (neighbour dirt) =$ exceed 32 $> state Grass, snowFall]

transformations Ice = [magmify, heat =$ exceed 2 $> state (Water Nothing)]

-- TODO: transfer heat properly when moving
transformations Snow =
        [ heat =$ exceed 1 $> state (Water Nothing)
        , wait (not . solid . down) $> state Air
        , count snowToIce =$ exceed 32 $> state Ice
        ]
  where
    snowToIce s =
        if Foldable.all (Foldable.all solid) s
        then neighbour snow s
        else 0
