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
                   ) where

import Prelewd

import Storage.Either

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

instance (Applicative m, Monad m) => Bind (Stream m) Maybe a b where
    bind s@(Stream f) = Stream $ \m -> extractNext <$> sequence (f <$> m)
        where
            extractNext Nothing = (Nothing, bind s)
            extractNext (Just (b, s')) = (b, bind s')

instance (Applicative m, Monad m) => Mappable (Stream m) Maybe a b where map = bind . map return

instance (Applicative m, Monad m) => Bind (Stream m) (Either r) a b where
    bind s@(Stream f) = Stream $ \e -> extractNext <$> sequence (f <$> e)
        where
            extractNext (Left r) = (Left r, bind s)
            extractNext (Right (b, s')) = (b, bind s')

instance (Applicative m, Monad m) => Mappable (Stream m) (Either r) a b where map = bind . map return

-- | Lift context from the output to the whole Stream
lift :: (Functor m, Monad m) => Stream Id a (m b) -> Stream m a b
lift (Stream f) = Stream $ recurse . runId . f
    where recurse (b, s) = b <&> (, lift s)

-- | Retype a Stream for Monadic use
identify :: (Functor m, Monad m) => Stream Id a b -> Stream m a b
identify (Stream f) = Stream $ return . runId . map (map identify) . f

-- | Repeatedly apply a function to an internal updating value
updater :: (a -> b -> b) -> b -> Stream Id (Maybe a) b
updater f b = Stream $ \m -> Id $ updater f <$> double (f <$> m <?> id $ b)
    where double x = (x, x)

-- | Maintain the most recent `Just`
latch :: Stream Id (Maybe a) (Maybe a)
latch = updater (\x _-> Just x) Nothing
