{-# LANGUAGE NoImplicitPrelude
           , FlexibleContexts
           , TemplateHaskell
           , TupleSections
           #-}
module Game.State ( GameState (..)
                  , Board
                  , tiles'
                  , game
                  ) where 

import Prelewd hiding ((!))

import Control.Stream
import Control.Stream.Either
import Data.Ix
import Data.Tuple
import Storage.Array
import Storage.Id
import Storage.List
import Storage.Pair
import Template.MemberTransformer

import Game.Input
import Game.Object
import Game.Vector
import Physics.Types

import Config

splitA :: Ix i => Array i (e, f) -> (Array i e, Array i f)
splitA a = listArray (bounds a) *** listArray (bounds a) $ unzip $ elems a

infix 3 <%%>

(<%%>) :: (Functor f, Functor g, Functor h) => f (g (a -> b)) -> h a -> f (g (h b))
(<%%>) fg h = (<$> h) <$$> fg

type Creation = (Object, Position)
type Board = Array Position Object
type GameUpdate = Either () Creation

newtype GameState = GameState { tiles :: Board }

$(memberTransformers ''GameState)

-- | Advance the GameState
game :: Stream Id (Maybe Input) GameState
game = bind updates >>> updater update (tiles initState, initGame) >>> arr (GameState . fst)

updates :: Stream Id Input (Maybe GameUpdate)
updates = arr reshape >>> map creations >>> arr sequence
    where
        reshape (Select o) = Right $ Left o
        reshape (Place p) = Right $ Right p
        reshape Step = Left ()

-- | What to add into the game world
creations :: Stream Id (Either Object Position) (Maybe Creation)
creations = liftA2 (,) <$> (buffer <<< lefts) <*> rights
    where
        buffer = updater (\x _-> Just x) Nothing

update :: GameUpdate -> (Board, Array Position Update) -> (Board, Array Position Update)

update (Right (obj, p)) (b, a) = (b // [(p, obj)], a // [(p, object obj)])

update (Left _) (b, a0) = splitA $ zipAWith (runId <$$> ($<)) a0 disseminate
    where
        -- Propogate each Spawn to its neighbours, so each Position will have one Object from each neighbour
        disseminate :: Array Position Seeds
        disseminate = listArray (bounds b) $ neighbour <$> indices b <%> dimensions <%%> Pair False True

        -- Find a neighbour in a given direction, and return the Object they're spawning towards the base position
        neighbour :: Position -> Dimension -> Bool -> Maybe Object
        neighbour p dim pos = let p' = component' dim (iff pos (+) subtract 1) p
                              in mcond (inRange (bounds b) p') $ b!p'

initGame :: Array Position Update
initGame = object <$> tiles initState

initState :: GameState
initState = GameState $ array (0, 31) initBoard
