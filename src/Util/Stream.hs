{-# LANGUAGE NoImplicitPrelude
           , FlexibleInstances
           , MultiParamTypeClasses
           , TupleSections
           #-}
-- | Nonterminating Monadic coroutines
module Util.Stream ( Stream (..)
                   , (>>$)
                   , identify
                   , updater
                   , latch
                   , buffer
                   ) where

import Prelewd

import Data.Tuple

import Util.Id

infix 6 $<
infix 2 >>$

data Stream m a b = Stream { ($<) :: (a -> m (b, Stream m a b)) }

instance Functor m => Functor (Stream m a) where
    fmap f (Stream s) = Stream $ (f *** fmap f) <$$> s

instance (Functor m, Monad m) => Applicative (Stream m a) where
    pure x = Stream $ \_ -> return (x, pure x)
    (Stream f1) <*> (Stream f2) = Stream $ \x -> do
                        (v1, f1') <- f1 x
                        (v1 *** (f1' <*>)) <$> f2 x

instance Monad m => Identity (Stream m) where
    id = Stream $ return . (, id)

instance (Functor m, Monad m) => Compose (Stream m) where
    (Stream f1) <<< (Stream f2) = Stream $ \x -> do
                                (y, f2') <- f2 x
                                (f2' >>>) <$$> f1 y

instance (Functor m, Monad m) => Mappable (Stream m) ((,) a) b b' where
    map (Stream f) = Stream $ \(a, b) -> ((a,) *** map) <$> f b

instance (Applicative m, Monad m) => Mappable (Stream m) Maybe a b where
    map s@(Stream f) = Stream $ \m -> do
                    bs <- sequence (f <$> m)
                    return (fst <$> bs, map $ snd <$> bs <?> s)

-- | Retype a Stream for Monadic use
identify :: (Functor m, Monad m) => Stream Id a b -> Stream m a b
identify (Stream f) = Stream $ return . runId . map (map identify) . f

-- | Repeatedly apply a function to an internal updating value
updater :: (Functor m, Monad m) => (a -> b -> b) -> b -> Stream m a b
updater f b = identify $ Stream $ \x -> return $ ((,) <*> updater f) $ f x b

-- | `updater` supporting fluctuating inputs
latch :: (Functor m, Monad m) => (a -> b -> b) -> b -> Stream m (Maybe a) b
latch f = updater $ try . map f

-- | Maintain the most recent `Just`
buffer :: Stream Id (Maybe a) (Maybe a)
buffer = updater (<|>) Nothing

-- | Use a Monad as an infinite source of values
(>>$) :: (Functor m, Monad m) => m a -> Stream m a b -> Stream m () b
(>>$) m (Stream f) = Stream $ \_-> m >>= map (m >>$) <$$> f
