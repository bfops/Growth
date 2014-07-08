{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
module Game.Object.Transformation ( UpdatePosition (..)
                                  , TileUpdate (..)
                                  , TileUpdater
                                  , BoardUpdate
                                  , toPosition
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
import Game.Vector
import Physics.Types

type TileUpdater m = Sink TileUpdateIn m

-- needs to be manually scaled for > 2 dimensions
-- | @Position@ used for updating the @Board@. Allows for positioning relative
-- to the current tile.
data UpdatePosition
    = Absolute Position
    | This
    | UpOf UpdatePosition
    | DownOf UpdatePosition
    | RightOf UpdatePosition
    | LeftOf UpdatePosition

-- TileUpdate functions defined in Game.State
infixl 1 :&
data TileUpdate
    = UpdateObject (Object -> Object)
    | UpdateHeat (Heat -> Heat)
    | RemakeTile Object
    | (:&) TileUpdate TileUpdate

type BoardUpdate = [(UpdatePosition, TileUpdate)]

toPosition :: Position -> UpdatePosition -> Position
toPosition _ (Absolute p) = p
toPosition p0 This = p0
toPosition p0 (UpOf p) = toPosition p0 p + Vector 0 1
toPosition p0 (DownOf p) = toPosition p0 p + Vector 0 (-1)
toPosition p0 (RightOf p) = toPosition p0 p + Vector 1 0
toPosition p0 (LeftOf p) = toPosition p0 p + Vector (-1) 0

this :: TileUpdate -> BoardUpdate
this t = [(This, t)]

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

neighbour :: (Maybe Tile -> Bool) -> TileUpdateIn -> Integer
neighbour n = mapsum (mapsum $ \x -> if n x then 1 else 0) . neighbours

whenHeat :: Monad m => (Heat -> Heat -> Bool) -> Heat -> TileUpdater m ()
whenHeat cond h = awaitUntil (\i -> cond (heatIn i) h) ()

heatTo :: Monad m => Heat -> TileUpdater m ()
heatTo = whenHeat (>=)

heatThisTo :: Monad m => Object -> TileUpdater m BoardUpdate
heatThisTo obj = heatTo (initHeat obj) $> this (UpdateObject (\_-> obj))

coolTo :: Monad m => Heat -> TileUpdater m ()
coolTo = whenHeat (<=)

coolThisTo :: Monad m => Object -> TileUpdater m BoardUpdate
coolThisTo obj = coolTo (initHeat obj) $> this (UpdateObject (\_-> obj))

-- | Get info about the water flowing through this tile from info about the neighbors.
waterFlow :: TileUpdateIn -> Maybe Flow
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

-- | Water will flow through (and occupy) these tiles
acceptWater :: Monad m => TileUpdater m BoardUpdate
acceptWater = do
    neighbors <- waterFlow <$> awaitJust
    case neighbors of
      Nothing -> acceptWater
      Just w -> return $ this $ RemakeTile $ Water w

volcano :: Maybe Tile -> Bool
volcano = maybe False ((== Lava True) . view tileObject)

-- | If there's a volcano under us, turn into lava.
joinVolcano :: Monad m => TileUpdater m BoardUpdate
joinVolcano = wait (volcano . down) $> this (UpdateObject $ \_-> Lava False)

transformations :: Monad m => Object -> [TileUpdater m BoardUpdate]

transformations Fire = [heatTo (initHeat (Lava False) / 4) $> this (UpdateObject $ \_-> Lava False), acceptWater]

transformations Grass =
    [ heatTo (initHeat Fire / 16) $> this (RemakeTile Fire)
    ]

transformations (Water s)
    = let flowOrNull = maybe [] (\_-> [flow]) s
      in [coolThisTo Ice, steamify] <> flowOrNull
  where
    flow = do
      f <- waterFlow <$> awaitJust
      case f of
        -- This is a non-source block and no more water is flowing through this tile.
        Nothing -> return $ this (UpdateObject $ \_-> Air)
        Just s' ->
          if s == s'
          then flow
          else return $ this (UpdateObject $ \_-> Water s')

    steamify = heatTo (initHeat Fire) $> this (UpdateObject $ \_-> Air)

transformations (Lava isVolcano)
    = [ wait makeVolcano $> this (UpdateObject $ \_-> Lava True)
      ]
    <> (if isVolcano then [] else [coolTo (initHeat Fire) $> this (UpdateObject $ \_-> Rock)])
  where
      makeVolcano s = lava (left s) && lava (right s)

transformations Rock =
        [ count (neighbour water) =$ wait (> 32) $> this (UpdateObject $ \_-> Dirt)
        ]

transformations Dirt = []

transformations Air =
    [ joinVolcano
    , acceptWater
    , count (neighbour dirt) =$ wait (>= 32) $> this (UpdateObject $ \_-> Grass)
    ]

transformations Ice = [heatThisTo (Water Nothing)]

-- TODO: transfer heatIn properly when moving
transformations Snow =
        [ heatThisTo (Water Nothing)
        , fall
        , count snowToIce =$ wait (>= 32) $> this (UpdateObject $ \_-> Ice)
        ]
  where
    fall = do
      i <- awaitJust
      case down i of
        Nothing -> fall
        Just footing ->
          if Foldable.any ($ Just footing) [water, air]
          then return $ [(This, UpdateObject $ \_-> Air), (DownOf This, UpdateObject (\_-> Snow) :& UpdateHeat (\_-> heatIn i))]
          else fall

    snowToIce s =
        if Foldable.all (Foldable.all solid) $ neighbours s
        then neighbour snow s
        else 0

transformations Basalt =
        [ heatTo (initHeat Basalt + initHeat Fire / 4) $> this (RemakeTile $ Lava False)
        ]
