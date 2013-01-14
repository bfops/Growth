{-# LANGUAGE NoImplicitPrelude
           #-}
module Game.State
                 
               where 

import Prelewd hiding ((!))

import Data.Array.IArray
import Data.Tuple
import Storage.List

import Game.Input
import Game.Object
import Physics.Types
import Util.Id
import Util.Stream

type Creation = (Object, Position)

newtype GameState = GameState { tiles :: Array Position (Maybe Object) }

fromPlace :: Input -> Maybe Position
fromPlace (Place p) = Just p
fromPlace _ = Nothing

fromSelect :: Input -> Maybe Object
fromSelect (Select obj) = Just obj
fromSelect _ = Nothing

-- | Advance the GameState
initGame :: Stream Id [Input] GameState
initGame = creations
         >>> updater (flip $ foldr create) (GameState $ listArray (0, 15) $ repeat Nothing)

creations :: Stream Id [Input] [Creation]
creations = (objects &&& arr (map fromPlace))
          <&> uncurry zip
          <&> mapMaybe (uncurry $ liftA2 (,))

objects :: Stream Id [Input] [Maybe Object]
objects = updater (flip $ scanr updateObj . join . head) []
    where
        updateObj i obj = fromSelect i <|> obj

create :: Creation -> GameState -> GameState
create (obj, p) (GameState t) = GameState $ t // [(p, (t!p) <|> Just obj)]
