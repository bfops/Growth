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
import Control.DeepSeq
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Array as Array
import Data.Conduit
import Data.Conduit.List as Conduit
import Data.Conduit.ResumableSink
import Data.Either
import Data.Foldable as Foldable
import Data.Function
import Data.Hashable
import Data.Maybe as Maybe
import Data.OpenUnion
import Data.Pair
import Data.Typeable
import GHC.Generics

import Game.Input
import Game.Object.Transformation
import Game.Object.Type hiding (left, right)
import Game.Vector
import Physics.Types

import Config

unzipA :: Ix i => Array i (e, f) -> (Array i e, Array i f)
unzipA a = let (es, fs) = unzip $ elems a
           in (listArray (bounds a) es, listArray (bounds a) fs)

zipAWithIxM :: (Ix i, Eq i, Functor m, Monad m) => (i -> a -> b -> m c) -> Array i a -> Array i b -> m (Array i c)
zipAWithIxM f x y
    = let bs = assert (bounds x == bounds y) $ bounds x
      in listArray bs <$> zipWithM (\(i, a) (_, b) -> f i a b) (assocs x) (assocs y)

zipAWith :: (Ix i, Eq i) => (a -> b -> c) -> Array i a -> Array i b -> Array i c
zipAWith f x y
    = let bs = assert (bounds x == bounds y) $ bounds x
      in listArray bs $ zipWith (\(_, a) (_, b) -> f a b) (assocs x) (assocs y)

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

type Transformations m = [ResumableSink m TileUpdateIn BoardUpdate]

resumeSink :: Monad m => [i] -> ResumableSink m i r -> m (Either r (ResumableSink m i r))
resumeSink is s = sourceList is ++$$ s

newtype GameState = GameState { tiles :: Board Tile }
  deriving (Show, Read, Eq)

-- | A `Tile` coupled with an updater.
data UpdateTile m = UpdateTile
    { getTile :: Tile
    , getTransformations :: Transformations m
    }

makeUpdateTile :: Monad m => Tile -> UpdateTile m
makeUpdateTile t = UpdateTile
    { getTile = t
    , getTransformations = newResumableSink <$> transformations (view tileObject t)
    }

applyUpdate :: Monad m => TileUpdate -> UpdateTile m -> UpdateTile m
applyUpdate (UpdateObject f) t = makeUpdateTile $ over tileObject f $ getTile t
applyUpdate (UpdateHeat f) t = t { getTile = over tileHeat f $ getTile t }
applyUpdate (RemakeTile obj) _ = makeUpdateTile $ makeTile obj
applyUpdate (u1 :& u2) t = applyUpdate u1 $ applyUpdate u2 t

-- | Advance the GameState
game :: (Applicative m, Monad m) => Conduit (Union '[Input, Tick]) m GameState
game
    = toGameUpdates
  =$= void (scanM updateBoard initGame)
  =$= Conduit.map (GameState . fmap getTile)

toGameUpdates :: Monad m => Conduit (Union '[Input, Tick]) m (Union '[GameUpdate, Tick])
toGameUpdates = void (mapAccum (toUpdate @> tick @> typesExhausted) Nothing) =$= Conduit.catMaybes
  where
    toUpdate (Select o) _ = (Just o, Nothing)
    toUpdate (Place p) o = (o, liftUnion . Create p <$> o)
    toUpdate Step o = (o, Just $ liftUnion UpdateStep)

    tick Tick o = (o, Just $ liftUnion Tick)

-- | Apply a single update operation to the board(s).
updateBoard ::
    (Applicative m, Monad m) =>
    Union '[GameUpdate, Tick] ->
    Board (UpdateTile m) ->
    m (Board (UpdateTile m))
updateBoard = force $ updateGame @> updateTick @> typesExhausted
  where
    -- identity event just to force re-evaluation down the line.
    -- (e.g. a re-rendering may occur because we yield again).
    updateTick Tick a = return a

    updateGame (Create p obj) b = return $ b // [(p, makeUpdateTile (makeTile obj))]
    updateGame UpdateStep b = do
          let
            -- Propogate each tile to its neighbours.
            inputs :: Board (Neighbours Tile)
            inputs = listArray (bounds b) $ neighbour <$> Array.indices b <%> dimensions <%%> Pair False True
          (b', changes) <- unzipA <$> zipAWithIxM updateTile inputs b
          let
            changeArray :: Board (Maybe TileUpdate)
            changeArray = Foldable.foldr setPos (listArray (bounds b') (repeat Nothing)) $ Foldable.concat changes
          return $ zipAWith makeChanges b' changeArray
        where
            -- Find the Tile in the given direction.
            neighbour :: Position -> Dimension -> Bool -> Maybe Tile
            neighbour p dim pos = let p' = over (component dim) (if pos then (+ 1) else subtract 1) p
                                  in guard (inRange (bounds b) p') >> Just (getTile $ b!p')

            setPos (p, v) a = a // [(p, Just v)]

            makeChanges t Nothing = t
            makeChanges t (Just u) = applyUpdate u t

-- TODO: finalize ResumableSinks properly
-- TODO: Remove conduits upon exhaustion.
updateTile ::
    (Monad m, Applicative m) =>
    Position ->
    Neighbours Tile ->
    UpdateTile m ->
    m (UpdateTile m, [(Position, TileUpdate)])
updateTile p0 ns ut0 = do
    let heat' = updateHeat ns (getTile ut0)
    let t' = set tileHeat heat' (getTile ut0)
    let i = TileUpdateIn t' ns
    (changes, transformers')
        <- partitionEithers <$> traverse (resumeSink [i]) (getTransformations ut0)
    let ut' = ut0
              { getTile = t'
              , getTransformations = transformers'
              }
    return (ut', [(toPosition p0 p, x) | (p, x) <- Foldable.concat changes])

average :: Fractional a => [a] -> a
average l = Foldable.sum l / realToFrac (length l)

updateHeat :: Neighbours Tile -> Tile -> Heat
updateHeat ns current =
    let flatNeighbours = Maybe.catMaybes $ Foldable.concatMap toList ns
    in view tileHeat current + realToFrac (average $ dE <$> flatNeighbours)
  where
      -- the amount of energy to change by to move toward temperature equilibrium
      -- with another tile.
      dE :: Tile -> Double
      dE neighbour =
        let
          totalHeatE = ((+) `on` realToFrac . view tileHeat) neighbour current
          relativeCapacity = ((/) `on` realToFrac . capacity . view tileObject) neighbour current
          -- the amount of heat energy `current` would have at temperature equilbrium with `neighbour`
          equilibrium = totalHeatE / (1 + relativeCapacity)
          delta = equilibrium - realToFrac (view tileHeat current)
          res = ((*) `on` realToFrac . resistence . view tileObject) neighbour current
        in delta / res

initGame :: (Applicative m, Monad m) => Board (UpdateTile m)
initGame = makeUpdateTile <$> initBoard
