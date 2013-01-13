{-# LANGUAGE NoImplicitPrelude
           , FlexibleInstances
           , MultiParamTypeClasses
           , TupleSections
           #-}
module Util.Stream ( Stream (..)
                   , updater
                   ) where

import Prelewd


infix 6 $<

data Stream a b = Stream { ($<) :: (a -> (b, Stream a b)) }

instance Functor (Stream a) where
    fmap = liftA

instance Applicative (Stream a) where
    pure x = Stream $ \_ -> (x, pure x)
    (Stream f1) <*> (Stream f2) = Stream $ \x -> let
                        (v1, f1') = f1 x
                        (v2, f2') = f2 x
                    in (v1 v2, f1' <*> f2')

instance Identity Stream where
    id = Stream (, id)

instance Compose Stream where
    (Stream f1) <<< (Stream f2) = Stream $ \x -> let
                                (y, f2') = f2 x
                            in f1 y <&> (<<< f2')


instance Mappable Stream ((,) a) b b' where
    map (Stream f) = Stream $ \(a, b) -> ((a,) *** map) $ f b

updater :: (a -> b -> b) -> b -> Stream a b
updater f b = Stream $ \x -> ((,) <*> updater f) $ f x b
