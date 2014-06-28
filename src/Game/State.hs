{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Game.State ( GameState (..)
                  , game
                  ) where 

import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Array as Array
import Data.Conduit
import Data.Conduit.List as Conduit
import Data.Hashable
import Data.OpenUnion
import Data.Pair
import Data.Typeable
import GHC.Generics

import Data.Conduit.Extra
import Game.Input
import Game.Object
import Game.Object.Type hiding (left, right)
import Game.Vector
import Physics.Types

import Config

unzipA :: Ix i => Array i (e, f) -> (Array i e, Array i f)
unzipA a = let (es, fs) = unzip $ elems a
           in (listArray (bounds a) es, listArray (bounds a) fs)

zipAWithM :: (Ix i, Eq i, Functor m, Monad m) => (a -> b -> m c) -> Array i a -> Array i b -> m (Array i c)
zipAWithM f x y
    = let bs = assert (bounds x == bounds y) $ bounds x
      in listArray bs <$> zipWithM (\(_, a) (_, b) -> f a b) (assocs x) (assocs y)

infixl 4 <%>, <%%>

(<%>) :: (Functor f, Functor g) => f (a -> b) -> g a -> f (g b)
(<%>) f g = (<$> g) <$> f

(<%%>) :: (Functor f, Functor g, Functor h) => f (g (a -> b)) -> h a -> f (g (h b))
(<%%>) fg h = fmap (<$> h) <$> fg

data GameUpdate
    = UpdateStep
    | Create Position Object
  deriving (Show, Read, Eq, Generic, Typeable)

instance Hashable GameUpdate

-- TODO: Remove conduits upon exhaustion.
stepAll ::
    (Applicative m, Monad m, Ix a, Eq a) =>
    Array a i ->
    Array a (ResumableConduit i m o) ->
    m (Array a o, Array a (ResumableConduit i m o))
stepAll is conduits = do
    (cs, os) <- unzipA <$> zipAWithM (exhaustInput . yield) is conduits
    return $ (fromSingle <$> os, cs)
  where
    fromSingle [x] = x
    fromSingle _ = error "fromSingle"

newtype GameState = GameState { tiles :: Board }
  deriving (Show, Read, Eq)

$(makeLenses ''GameState)

-- | Advance the GameState
game :: (Applicative m, Monad m) => Conduit (Union '[Input, Tick]) m GameState
game = updates =$= void (mapAccumM updateStep (initBoard, initGame))
  where
    updateStep x y = do
      (b, a) <- update x y
      return ((b, a), GameState b)

updates :: Monad m => Conduit (Union '[Input, Tick]) m (Union '[GameUpdate, Tick])
updates = void (mapAccum (toUpdate @> tick @> typesExhausted) Nothing) =$= Conduit.catMaybes
  where
    toUpdate (Select o) _ = (Just o, Nothing)
    toUpdate (Place p) o = (o, liftUnion . Create p <$> o)
    toUpdate Step o = (o, Just $ liftUnion UpdateStep)

    tick Tick o = (o, Just $ liftUnion Tick)

update ::
    (Applicative m, Monad m) =>
    Union '[GameUpdate, Tick] ->
    (Board, Array Position (Update m)) ->
    m (Board, Array Position (Update m))
update = updateGame @> updateTick @> typesExhausted
  where
    updateTick Tick (b, a) = return (b, a)

    updateGame (Create p obj) (b, a) = return $ (b // [(p, Tile obj)], a // [(p, tile obj)])

    updateGame UpdateStep (b, a) = stepAll disseminate a
        where
            -- Propogate each Spawn to its neighbours, so each Position will have one Object from each neighbour
            disseminate :: Array Position (Neighbours Tile)
            disseminate = let
                in listArray (bounds b) $ neighbour <$> Array.indices b <%> dimensions <%%> Pair False True

            -- Find a neighbour in a given direction, and return the Object they're spawning towards the base position
            neighbour :: Position -> Dimension -> Bool -> Maybe Tile
            neighbour p dim pos = let p' = over (component dim) (if pos then (+ 1) else subtract 1) p
                                  in guard (inRange (bounds b) p') >> Just (b!p')

initGame :: (Applicative m, Monad m) => Array Position (Update m)
initGame = tile . objType <$> initBoard
