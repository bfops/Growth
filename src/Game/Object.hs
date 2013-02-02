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

count :: Foldable t => (a -> Bool) -> t a -> Integer
count p = foldr (\x -> if' (p x) (+ 1)) 0

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
        precList = [ Water True, Water False, Lava False, Fire, Grass, Rock, Air, Lava True ]

-- | Combine one object into another
mix :: Dimension -> Bool -> Object -> Object -> Object

mix _ _ Air (Lava False) = Rock

mix Height True Air (Water _) = Water False
mix Height True Rock (Water _) = Water True

mix _ _ Fire Grass = Fire
mix Height True Fire (Water _) = Water False

mix _ _ (Lava False) Grass = Fire
mix _ _ (Lava False) (Water _) = Air
mix _ _ (Lava False) Rock = Lava False

mix Height True (Lava True) Rock = Lava True
mix _ _ (Lava True) _ = Lava False

mix Height True (Water _) (Water _) = Water False
mix Height True (Water _) x = x
mix Width _ (Water False) x = x
mix Width _ (Water True) (Water _) = Water True
mix _ _ (Water b) Fire = Water b
mix _ _ (Water b) Air = Water b

mix _ _ Grass (Water _) = Grass

mix _ _ _ x = x

object :: Update
object = arr Just >>> updater mixNeighbours Air

mixPrecedence :: Seeds -> Object -> Object
mixPrecedence v obj = foo $ toList ((,) <$$> (mix <$> dimensions <%> Pair True False) <**> v) >>= toList
    where
        foo :: [(Object -> Object -> Object, Maybe Object)] -> Object
        foo = foldr (\(a, b) -> a b) obj . sortBy (compare `on` precedence obj . snd) . mapMaybe sequence

mixNeighbours :: Seeds -> Object -> Object
mixNeighbours v (Lava b) = iff (volcano v) (Lava True) $ mixPrecedence v $ Lava b
    where
        volcano (Vector (Pair (Just (Lava _)) (Just (Lava _))) (Pair (Just (Lava _)) (Just Rock))) = True
        volcano _ = False
mixNeighbours v Rock = if count (Just Fire ==) (toList v >>= toList) >= 3
                       then Lava True
                       else mixPrecedence v Rock
mixNeighbours v (Water b) = case (mixPrecedence v $ Water b, v) of
                                (Water _, Vector _ (Pair (Just (Water _)) _)) -> Water False
                                (x, _) -> x
mixNeighbours v obj = mixPrecedence v obj
