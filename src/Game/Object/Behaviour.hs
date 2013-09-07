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

import Summit.Control.Stream
import Summit.Data.Id
import Summit.Data.Member
import Summit.Impure
import Summit.Prelewd

import Data.Tuple

import Game.Object.Type

type Behaviour = Stream Maybe Seeds ()

wait :: Stream Id Seeds Bool -> Behaviour
wait s = lift $ s >>> arr (\b -> mcond (not b) ())

infixl 2 `except`
-- | Create a Behaviour which, upon termination, will first check some other Behaviour for confirmation.
-- Note that both Behaviours advance every time the resultant Behaviour advances.
except :: Behaviour     -- ^ Terminating Behaviour
       -> Behaviour     -- ^ Upon termination, check this Behaviour.
                        -- If it also terminates, refute the termination of the original Behaviour.
       -> Behaviour
b `except` ex = Stream $ \seeds -> case (b $< seeds, ex $< seeds) of
                            (Nothing, Just _) -> Nothing
                            (b', ex') -> Just ((), snd <$> b' <?> b `except` snd <$> ex' <?> ex)

-- | Accumulate a count until a specified goal
count :: Integer -> (Seeds -> Integer) -> Behaviour
count 0 _ = error "count 0 causes instant change"
count n c = wait
          $ folds (barr $ newCount . c) 0
        >>> arr (iff (n > 0) (>= n) (<= n))
    where
        newCount d i = try (iff (n > 0) max min) 0 $ d + i

neighbour :: (Maybe Object -> Bool) -> Seeds -> Integer
neighbour n = foldr (flip $ foldr $ \x -> if' (n x) (+ 1)) 0

mix :: Object -> Behaviour
mix = wait . arr . any . elem . Just

heat :: Integer -> Behaviour
heat n = count n $ \s -> sum $ sum . map (\o -> deltaHeat <$> o <?> 0) <$> s
    where
        deltaHeat Fire = 2
        deltaHeat (Lava b) = iff b 12 8
        deltaHeat Ice = -1
        deltaHeat _ = 0
