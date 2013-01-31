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
            | Lava
            | Grass
            | Water
            | Air
            | Rock
    deriving (Show, Eq)

-- | Spawns from a set of neighbours
type Seeds = Vector (Pair (Maybe Object))
type Update = Stream Id Seeds Object

count :: (Foldable t, Eq a) => a -> t a -> Integer
count x = foldr (\a -> if' (a == x) (+ 1)) 0

precedence :: Object -> Object -> Int
precedence _ obj = length precList - (elemIndex obj precList <?> error ("No precedence for " <> show obj))
    where precList = [ Lava, Water, Fire, Grass, Rock, Air ]

-- | Combine one object into another
mix :: Dimension -> Bool -> Object -> Object -> Object

mix _ _ Air Lava = Rock
mix _ _ _ Lava = Lava

mix _ _ Lava Rock = Lava
mix _ _ _ Rock = Rock

mix _ _ Air x = x
mix _ _ Rock x = x

mix _ _ Fire Fire = Fire
mix _ _ Fire Grass = Fire
mix _ _ Fire Water = Water
mix _ _ Fire Air = Air

mix _ _ Lava Fire = Fire
mix _ _ Lava Grass = Fire
mix _ _ Lava Water = Air
mix _ _ Lava Air = Air

mix _ _ Water Grass = Grass
mix Height True Water x = x
mix _ _ Water Water = Water
mix _ _ Water Air = Water
mix _ _ Water Fire = Water

mix _ _ Grass Fire = Fire
mix _ _ Grass Grass = Grass
mix _ _ Grass Water = Grass
mix _ _ Grass Air = Air

object :: Update
object = arr Just >>> updater mixNeighbours Air

mixNeighbours :: Seeds -> Object -> Object
mixNeighbours v obj =
            -- Rock mostly surrounded by Fire -> Lava
            if obj == Rock && count (Just Fire) (toList v >>= pair (\x y -> [x, y])) >= 3
            then Lava
            else mixPrecedence $ toList ((,) <$$> (mix <$> dimensions <%> Pair True False) <**> v) >>= toList
    where
        mixPrecedence :: [(Object -> Object -> Object, Maybe Object)] -> Object
        mixPrecedence = foldr (\(a, b) -> a b) obj . sortBy (compare `on` precedence obj . snd) . mapMaybe sequence
