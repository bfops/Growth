{-# LANGUAGE NoImplicitPrelude
           , TemplateHaskell
           #-}
module Game.State ( GameState (..)
                  , tiles'
                  , game
                  ) where 

import Prelewd hiding ((!))

import Data.Ix
import Data.Tuple
import Storage.Array
import Storage.Either
import Storage.List
import Template.MemberTransformer

import Game.Input
import Game.Object
import Game.Vector
import Physics.Types
import Util.Id
import Util.Stream

lefts :: Stream Id (Either a b) (Maybe a)
lefts = arr $ either Just (\_-> Nothing)

rights :: Stream Id (Either a b) (Maybe b)
rights = arr $ either (\_-> Nothing) Just

sequence2 :: Applicative f => (f a, f b) -> f (a, b)
sequence2 = uncurry (liftA2 (,))

up, down, left, right :: Array Position (Vector (Object, Object)) -> Position -> Maybe Object

up = getObj Height True
down = getObj Height False
left = getObj Width False
right = getObj Width True

getObj :: Dimension -> Bool -> Array Position (Vector (Object, Object)) -> Position -> Maybe Object
getObj dim pos b p = iff pos fst snd . component dim . (b !)
                   <$> checkBounds (component' dim (iff pos (+) subtract 1) p)
    where
        checkBounds = let (l, h) = bounds b
                      in cast $ inRange (l, h)

type Creation = (Object, Position)
type Board = Array Position Object

newtype GameState = GameState { tiles :: Board }

$(memberTransformers ''GameState)

-- | Advance the GameState
game :: Stream Id (Maybe Input) GameState
game = bind updates >>> updater (either step place) initGame

reshape :: Input -> Either () (Either Object Position)
reshape (Select o) = Right $ Left o
reshape (Place p) = Right $ Right p
reshape Step = Left ()

updates :: Stream Id Input (Maybe (Either () Creation))
updates = arr reshape >>> map creations >>> arr sequence

-- | What to add into the game world
creations :: Stream Id (Either Object Position) (Maybe Creation)
creations = sequence2 <$> ((latch <<< lefts) &&& rights)

-- | Add something into the game world, if the tile isn't occupied
place :: Creation -> GameState -> GameState
place (obj, p) (GameState t) = GameState $ t // [(p, mix obj $ t!p)]

-- | Advance the game state
step :: () -> GameState -> GameState
step _ = tiles' $ mapWithIx . combine =<< map spawn
    where
        -- | Mix a tile with its surroundings.
        combine :: Array Position (Vector (Object, Object))  -> Position -> Object -> Object
        combine b p obj = foldr mixNeighbour obj $ [ up, left, right, down ]
            where
                mixNeighbour nbr = mix <$> nbr b p <?> id

initGame :: GameState
initGame = GameState $ listArray (0, 15) $ repeat Air
