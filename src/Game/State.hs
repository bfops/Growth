{-# LANGUAGE NoImplicitPrelude
           , FlexibleContexts
           , TemplateHaskell
           , TupleSections
           #-}
module Game.State ( GameState (..)
                  , tiles'
                  , game
                  ) where 

import Prelewd hiding ((!))

import Impure
import Text.Show

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
import Game.Object.Heat
import Game.Object.Type
import Game.Vector
import Physics.Types

import Config

splitA :: Ix i => Array i (e, f) -> (Array i e, Array i f)
splitA a = listArray (bounds a) *** listArray (bounds a) $ unzip $ elems a

infix 3 <%%>

(<%%>) :: (Functor f, Functor g, Functor h) => f (g (a -> b)) -> h a -> f (g (h b))
(<%%>) fg h = (<$> h) <$$> fg

type Creation = (Object, Position)
type GameUpdate = Either () Creation

newtype GameState = GameState { tiles :: Board }

$(memberTransformers ''GameState)

-- | Advance the GameState
game :: Stream Id (Maybe Input) GameState
game = bind updates
   >>> map (updater (barr update) (initBoard, initGame) >>> arr (GameState . fst))
   >>> latch (GameState initBoard)

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
        buffer = updater (barr (<|>)) Nothing

update :: GameUpdate -> (Board, Array Position Update) -> (Board, Array Position Update)

update (Right (obj, p)) (b, a) = (b // [(p, (obj, initHeat obj))], a // [(p, object obj)])

update (Left _) (b, a0) = traceShow "--------------------------"
                        $ traceShow (withTrace <$> showHeat boardDims)
                        $ splitA $ zipAWith (runId <$$> ($<)) a0 disseminate
    where
        showHeat (Vector mx my) = [ intercalate " " [ show $ snd $ b ! Vector x y
                                                    | x <- [0..mx-1]
                                                    ]
                                  | y <- reverse [0..my-1]
                                  ]

        -- Propogate each Spawn to its neighbours, so each Position will have one Object from each neighbour
        disseminate :: Array Position Neighbours
        disseminate = listArray (bounds b) $ neighbour <$> indices b <%> dimensions <%%> Pair False True

        -- Find a neighbour in a given direction, and return the Object they're spawning towards the base position
        neighbour :: Position -> Dimension -> Bool -> Maybe WarmObject
        neighbour p dim pos = let p' = component' dim (iff pos (+) subtract 1) p
                              in mcond (inRange (bounds b) p') $ b!p'

initGame :: Array Position Update
initGame = object . fst <$> initBoard
