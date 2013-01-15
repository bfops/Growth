{-# LANGUAGE NoImplicitPrelude
           , FlexibleInstances
           , MultiParamTypeClasses
           , TupleSections
           #-}
-- | Nonterminating Monadic coroutines
module Util.Stream ( Stream (..)
                   , lift
                   , identify
                   , updater
                   , latch
                   , buffer
                   ) where

import Prelewd

import Util.Id

infix 6 $<

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
    map = bind . map return

instance (Applicative m, Monad m) => Bind (Stream m) Maybe a b where
    bind s@(Stream f) = Stream $ \m -> extractNext <$> sequence (f <$> m)
        where
            extractNext Nothing = (Nothing, bind s)
            extractNext (Just (b, s')) = (b, bind s')

-- | Lift context from the output to the whole Stream
lift :: (Functor m, Monad m) => Stream Id a (m b) -> Stream m a b
lift (Stream f) = Stream $ (\(b, s) -> b <&> (, lift s)) . runId . f

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
