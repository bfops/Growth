{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
-- | Basic game object type, and associated functions
module Game.Object ( Object (..)
                   , Seeds
                   , Update
                   , object
                   , mix
                   ) where

import Prelewd hiding (all)

import Impure

import Control.Stream
import Data.Tuple
import Storage.Id
import Storage.List
import Storage.Pair
import Text.Show

import Game.Vector

data Object = Fire
            | Lava Bool
            | Grass
            | Water Bool
            | Air
            | Rock
    deriving (Show, Eq)

-- | Spawns from a set of neighbours
type Seeds = Vector (Pair (Maybe Object))
type Update = Stream Id Seeds Object

precedence :: Object -> Object -> Int
precedence _ obj = length precList - (elemIndex obj precList <?> error ("No precedence for " <> show obj))
    where
        precList = [ Lava False, Water True, Water False, Fire, Grass, Rock, Air, Lava True ]

-- | Combine one object into another
mix :: Dimension -> Bool -> Object -> Object -> Object

mix _ _ Air (Lava False) = Rock
mix Height True (Lava True) (Lava _) = Lava False
mix _ _ _ (Lava b) = Lava b

mix Height True (Lava True) Rock = Lava True
mix _ _ (Lava _) Rock = Lava False
mix _ _ _ Rock = Rock

mix Height True Air (Water _) = Water False
mix _ _ Air x = x
mix Height True Rock (Water _) = Water True
mix _ _ Rock x = x

mix _ _ Fire Fire = Fire
mix _ _ Fire Grass = Fire
mix Height True Fire (Water _) = Water False
mix _ _ Fire (Water b) = Water b
mix _ _ Fire Air = Air

mix _ _ (Lava False) Fire = Fire
mix _ _ (Lava False) Grass = Fire
mix _ _ (Lava False) (Water _) = Air
mix _ _ (Lava False) Air = Air

mix _ _ (Lava True) _ = Lava False

mix _ _ (Water _) Grass = Grass
mix Height True (Water _) (Water _) = Water False
mix Height True (Water _) x = x
mix Height False (Water _) (Water b) = Water b
mix Width _ (Water False) x = x
mix Width _ (Water True) (Water _) = Water True
mix _ _ (Water b) _ = Water b

mix _ _ Grass Fire = Fire
mix _ _ Grass Grass = Grass
mix _ _ Grass (Water _) = Grass
mix _ _ Grass Air = Air

object :: Update
object = arr Just >>> updater mixNeighbours Air

mixNeighbours :: Seeds -> Object -> Object
mixNeighbours v obj =
        -- Rock mostly surrounded by Fire -> Lava
        if obj == Rock && length (filter (Just Fire ==) $ toList v >>= toList) >= (3 :: Integer)
        || obj == Lava False && volcano v
        then Lava True
        else mixPrecedence $ toList ((,) <$$> (mix <$> dimensions <%> Pair True False) <**> v) >>= toList
    where
        mixPrecedence :: [(Object -> Object -> Object, Maybe Object)] -> Object
        mixPrecedence = foldr (\(a, b) -> a b) obj . sortBy (compare `on` precedence obj . snd) . mapMaybe sequence

        volcano (Vector (Pair (Just (Lava _)) (Just (Lava _))) (Pair (Just (Lava _)) (Just Rock))) = True
        volcano _ = False
