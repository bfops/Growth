{-# LANGUAGE NoImplicitPrelude
           , DeriveFunctor
           #-}
-- | Type-level identity
module Util.Id ( Id (..)
               ) where

import Prelewd

import Text.Show

-- | Identity Monad
data Id a = Id { runId :: a }
    deriving (Show, Eq)

-- Instances avoid pattern matching, because it plays poorly with recursive Monads, e.g.

-- data Stream m a b = Stream { ($<) :: a -> m (b, Stream m a b) }
--
-- fromStream :: Stream m a b -> [a] -> m [b]
-- fromStream s (x:xs) = do
--            (r, s') <- s $< x
--            (r:) <$> fromStream s' xs
-- fromStream _ _ = return []
--
-- ones :: Num a => Stream Id () a
-- ones = Stream $ \_-> Id (1, ones)
--
-- take 10 $ runId $ fromStream (Stream ones) $ repeat () -- This doesn't converge with pattern matching

instance Monad Id where
    return = Id
    x >>= f = f $ runId x

instance Applicative Id where
    pure = return
    (<*>) = ap

instance Functor Id where fmap = liftA
