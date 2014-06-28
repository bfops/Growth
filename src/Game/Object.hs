{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Basic game object type, and associated functions
module Game.Object ( Tile (..)
                   , tile
                   , Neighbours
                   , Update
                   , Board
                   ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Array
import Data.Conduit
import Data.Conduit.List
import Data.Conduit.ResumableSink
import Data.Either
import Data.Pair
import GHC.Generics

import Game.Object.Type
import Game.Object.Transformation
import Game.Vector
import Physics.Types

resumeSink :: Monad m => [i] -> ResumableSink m i r -> m (Either r (ResumableSink m i r))
resumeSink is s = sourceList is ++$$ s

data Tile = Tile
    { objType :: Object
    }
  deriving (Show, Read, Eq, Ord, Generic)

type Neighbours v = Vector (Pair (Maybe v))
type Update m = ResumableConduit (Neighbours Tile) m Tile
type Board = Array Position Tile
type Transformations m = [ResumableSink m (Neighbours Object) NewState]

tileState :: Monad m => Object -> (Tile, Transformations m)
tileState obj = (Tile obj, newResumableSink <$> transformations obj)

tile :: (Applicative m, Monad m) => Object -> Update m
tile obj = newResumableConduit $ void $ mapAccumM updateAndProduceTile (tileState obj)
  where
    updateAndProduceTile a b = updateTile a b <&> \(t', ts') -> ((t', ts'), t')

-- TODO: finalize ResumableSinks properly
updateTile ::
    (Monad m, Applicative m) =>
    Neighbours Tile ->
    (Tile, Transformations m) ->
    m (Tile, Transformations m)
updateTile ns (t, transformers) = do
    (objChanges, transformers')
        <- partitionEithers <$> traverse (resumeSink [seeds ns]) transformers
    case objChanges of
      [] -> return (t, transformers')
      (State obj rerun:_) ->
          (if rerun then updateTile ns else return) (tileState obj)
  where
    seeds :: Neighbours Tile -> Neighbours Object
    seeds = fmap $ fmap $ fmap objType
