{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
module Game.Object.Transformation ( Transformation
                                  , NewState (..)
                                  , transformations
                                  ) where

import Control.Applicative
import Control.Lens ((<&>))
import Control.Monad
import Data.Conduit as Conduit
import Data.Functor
import Data.Foldable as Foldable
import Data.Monoid

import Data.Conduit.Extra as Conduit
import Game.Object.Behaviour
import Game.Object.Type

data NewState
    = State Object Bool
  deriving (Show, Read, Eq, Ord)

state :: Object -> NewState
state o = State o False

rerun :: Object -> NewState
rerun o = State o True

type Neighbours = Seeds
type Transformation m = Sink Neighbours m NewState

-- | Get info about the water flowing through this tile from info about the neighbors.
waterFlow :: Neighbours -> Maybe Flow
waterFlow s
    = let
        below = down s
        above = up s
        l = rightWater $ left s
        r = leftWater $ right s
      in do
        guard $ water above || l || r
        Just $ Just $
          if not (solid below)
          then (False, False)
          else
            if water above
            then (True, True)
            else (not l, not r)

-- | Incorporate flowing Water
hydrophilic :: Neighbours -> Bool
hydrophilic = maybe False (\_-> True) . waterFlow

-- | Water will flow through (and occupy) these tiles
waterThrough :: Monad m => Transformation m
waterThrough = do
    neighbors <- waterFlow <$> awaitJust
    case neighbors of
      Nothing -> waterThrough
      Just w -> return $ state $ Water w

magmify :: Monad m => Transformation m
magmify = wait (mix $ Lava True) $> state (Lava False)

-- | The conditions for Lava to convert to Rock
lavaToRock :: Neighbours -> Bool
lavaToRock s = Foldable.any ($ s) [hydrophilic, mix Air]

snowFall :: Monad m => Transformation m
snowFall = wait (snow . up) $> state Snow

transformations :: Monad m => Object -> [Transformation m]

transformations Fire = [magmify, waterThrough]
transformations Grass =
    [ magmify
    , wait (mix Fire) $> state Fire
    , wait (mix $ Lava False) $> state Fire
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
        volcano = wait ((Just (Lava True) ==) . down) $> state (Lava True)

transformations Dirt = [wait (mix $ Lava False) $> state (Lava False), magmify]

transformations Air = [magmify, waterThrough, count (neighbour dirt) =$ exceed 32 $> state Grass, snowFall]

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
