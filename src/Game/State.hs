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

up, down, left, right :: Position -> Position
up = component' Height (+1)
down = component' Height (subtract 1)
left = component' Width (subtract 1)
right = component' Width (+1)

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
place (obj, p) (GameState t) = GameState $ t // [(p, mix (t!p) obj)]

-- | Advance the game state
step :: () -> GameState -> GameState
step _ = tiles' $ mapWithIx . combine =<< map spawn
    where
        -- | Mix a tile with its surroundings.
        combine :: Board -> Position -> Object -> Object
        combine b p obj = foldr mixNeighbour obj $ [ up, left, right, down ]
            where
                mixNeighbour nbr = mix . (b !) <$> checkBounds (nbr p) <?> id

                checkBounds = let (l, h) = bounds b
                              in cast $ inRange (l, h)

initGame :: GameState
initGame = GameState $ listArray (0, 15) $ repeat Air
