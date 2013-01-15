{-# LANGUAGE NoImplicitPrelude
           #-}
module Game.State ( GameState
                  , tiles
                  , game
                  ) where 

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
type Board = Array Position (Maybe Object)

newtype GameState = GameState Board

tiles :: GameState -> Board
tiles (GameState b) = b

fromSelect :: Input -> Maybe Object
fromSelect (Select obj) = Just obj
fromSelect _ = Nothing

fromPlace :: Input -> Maybe Position
fromPlace (Place p) = Just p
fromPlace _ = Nothing

sequence2 :: Applicative f => (f a, f b) -> f (a, b)
sequence2 = uncurry (liftA2 (,))

-- | Advance the GameState
game :: Stream Id (Maybe Input) GameState
game = bind creations >>> updater place (GameState $ listArray (0, 15) $ repeat Nothing)

-- | What to add into the game worl
creations :: Stream Id Input (Maybe Creation)
creations = sequence2 <$> ((latch <<< arr fromSelect) &&& arr fromPlace)

-- | Add something into the game world, if the tile isn't occupied
place :: Creation -> GameState -> GameState
place (obj, p) (GameState t) = GameState $ t // [(p, (t!p) <|> Just obj)]
