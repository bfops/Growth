{-# LANGUAGE NoImplicitPrelude
           , FlexibleContexts
           , TemplateHaskell
           #-}
module Game.State ( GameState (..)
                  , tiles'
                  , game
                  ) where 

import Prelewd hiding ((!))

import Control.Stream
import Data.Ix
import Data.Tuple
import Storage.Array
import Storage.Either
import Storage.Id
import Storage.List
import Storage.Pair
import Template.MemberTransformer

import Game.Input
import Game.Object
import Game.Vector
import Physics.Types

lefts :: Stream Id (Either a b) (Maybe a)
lefts = arr $ either Just (\_-> Nothing)

rights :: Stream Id (Either a b) (Maybe b)
rights = arr $ either (\_-> Nothing) Just

sequence2 :: Applicative f => (f a, f b) -> f (a, b)
sequence2 = uncurry (liftA2 (,))

splitA :: Ix i => Array i (e, f) -> (Array i e, Array i f)
splitA a = listArray (bounds a) *** listArray (bounds a) $ unzip $ elems a

type Creation = (Object, Position)
type Board = Array Position (Object, Spawn)

newtype GameState = GameState { tiles :: Board }

$(memberTransformers ''GameState)

-- | Advance the GameState
game :: Stream Id (Maybe Input) GameState
game = bind updates >>> updater update (initBoard, initGame) >>> arr (GameState . fst)

updates :: Stream Id Input (Maybe (Either () Creation))
updates = arr reshape >>> map creations >>> arr sequence
    where
        reshape (Select o) = Right $ Left o
        reshape (Place p) = Right $ Right p
        reshape Step = Left ()

-- | What to add into the game world
creations :: Stream Id (Either Object Position) (Maybe Creation)
creations = sequence2 <$> ((buffer <<< lefts) &&& rights)
    where
        buffer = updater (\x _-> Just x) Nothing

update :: Either () Creation -> (Board, Array Position Update) -> (Board, Array Position Update)

update (Right (obj, p)) (b, a) = let Id (result, s) = a ! p $< Just (singleSpawn obj)
                                 in (b // [(p, result)], a // [(p, s)])
    where
        singleSpawn = singleV (pure Nothing) Width . Pair Nothing . Just

update (Left _) (b, a0) = splitA $ zipAWith (runId <$$> ($<)) a0 $ Just <$> disseminate
    where
        -- Propogate each Spawn to its neighbours, so each Position will have one Object from each neighbour
        disseminate :: Array Position Seeds
        disseminate = listArray (bounds b) $ seeds <$> indices b

        seeds p = dimensions <&> \d -> neighbour d <$> Pair False True <&> ($ p)

        -- Find a neighbour in a given direction, and return the Object they're spawning towards the base position
        neighbour dim pos p = do
                    (obj, spwn) <- (b !) <$> checkBounds b (component' dim (iff pos (+) subtract 1) p)
                    mcond (iff pos fst snd $ pair (,) $ component dim spwn) obj

checkBounds :: Board -> Position -> Maybe Position
checkBounds b = let (l, h) = bounds b
                in cast (inRange (l, h))

initGame :: Array Position Update
initGame = listArray (0, 15) $ repeat object

initBoard :: Board
initBoard = initGame <&> \_-> (Air, pure $ pure True)
