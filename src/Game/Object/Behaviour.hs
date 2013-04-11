{-# LANGUAGE NoImplicitPrelude
           #-}
module Game.Object.Behaviour ( Behaviour
                             , wait
                             , except
                             , count
                             , neighbour
                             , mix
                             , heat
                             ) where

import Prelewd

import Control.Stream
import Data.Tuple
import Storage.Id

import Physics.Types

import Game.Object.Type

-- | Wait for a value to surpass a target (in either direction)
exceed :: (Num a, Ord a)
       => a     -- ^ Target
       -> Stream Id a Bool
exceed target = loop (barr check) Nothing
    where
        check a direction = let b = direction <?> (a < target)
                            in (iff b (>=) (<=) a target, Just b)

type Behaviour = Stream Maybe (Seeds, Heat) ()

wait :: Stream Id (Seeds, Heat) Bool -> Behaviour
wait s = lift $ s >>> arr (\b -> mcond (not b) ())

infixl 2 `except`
-- | Create a Behaviour which, upon termination, will first check some other Behaviour for confirmation.
-- Note that both Behaviours advance every time the resultant Behaviour advances.
except :: Behaviour     -- ^ Terminating Behaviour
       -> Behaviour     -- ^ Upon termination, check this Behaviour.
                        -- If it also terminates, refute the termination of the original Behaviour.
       -> Behaviour
b `except` ex = Stream $ \info -> case (b $< info, ex $< info) of
                            (Nothing, Just _) -> Nothing
                            (b', ex') -> Just ((), snd <$> b' <?> b `except` snd <$> ex' <?> ex)

-- | Accumulate a count until a specified goal
count :: Integer -> (Seeds -> Integer) -> Behaviour
count n c = wait
          $ updater (barr $ newCount . c . fst) 0
        >>> exceed n
    where
        newCount d i = try (iff (n > 0) max min) 0 $ d + i

neighbour :: (Maybe Object -> Bool) -> Seeds -> Integer
neighbour n = foldr (flip $ foldr $ \x -> if' (n x) (+ 1)) 0

mix :: Object -> Behaviour
mix obj = wait $ arr $ any (any (== Just obj)) . fst

heat :: Heat -> Behaviour
heat target = wait $ arr snd >>> exceed target
