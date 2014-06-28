{-# LANGUAGE DoAndIfThenElse #-}
module Game.Object.Behaviour ( Behaviour
                             , wait
                             , exceed
                             , count
                             , neighbour
                             , mix
                             , heat
                             ) where

import Control.Monad
import Data.Conduit
import Data.Conduit.List as Conduit
import Data.Foldable as Foldable

import Data.Conduit.Extra
import Game.Object.Type

-- | Wait for a value to surpass a target (in either direction)
exceed :: (Num i, Ord i, Monad m) => i -> Sink i m ()
exceed target = do
    i <- awaitJust
    if signum i == signum target && abs i > abs target
    then return ()
    else exceed target

type Behaviour m = Sink Seeds m ()

-- | wait until the output becomes True.
wait :: Monad m => (i -> Bool) -> Sink i m ()
wait s = Conduit.map s =$= go
  where
    go = do
      b <- awaitJust
      if b then return () else go

-- | Accumulate a count.
count :: Monad m => (i -> Integer) -> Conduit i m Integer
count c = Conduit.map c =$= void (scan (+) 0)

mapreduce :: (Functor t, Foldable t) => (b -> b -> b) -> b -> (a -> b) -> t a -> b
mapreduce reduce b f = Foldable.foldl' reduce b . fmap f

mapsum :: (Functor t, Foldable t, Num b) => (a -> b) -> t a -> b
mapsum = mapreduce (+) 0

neighbour :: (Maybe Object -> Bool) -> Seeds -> Integer
neighbour n = mapsum $ mapsum $ \x -> if n x then 1 else 0

mix :: Object -> Seeds -> Bool
mix obj = Foldable.any $ Foldable.elem $ Just obj

-- | Keep track of accumulated heat from neighbors.
heat :: Monad m => Conduit Seeds m Integer
heat = count $ mapsum $ mapsum $ maybe 0 deltaHeat
  where
    deltaHeat Fire = 2
    deltaHeat (Lava True) = 12
    deltaHeat (Lava False) = 8
    deltaHeat Ice = -1
    deltaHeat _ = 0
