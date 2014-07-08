{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
-- | Static size, homogenous Vector type
module Game.Vector ( Vector (..)
                   , Dimension (..)
                   , dimensions
                   , component
                   , singleV
                   , Game.Vector.vector
                   , magnitude
                   , normalize
                   , dot
                   ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Foldable as Foldable
import Data.Function
import Data.Hashable
import Data.Ix
import Data.Maybe
import Data.Traversable as Traversable
import GHC.Generics

import Test.QuickCheck as QuickCheck

-- | Physical dimensions in the game
data Dimension = Width | Height
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Arbitrary Dimension where
    arbitrary = QuickCheck.elements [minBound..maxBound]

-- | Vector associating each dimension to a vector component
dimensions :: Vector Dimension
dimensions = Vector Width Height

-- | Homogenous vector. When possible, use predefined functions, rather than the Vector data constructor.
-- Code should strive to be generic with respect to the number of dimensions in a Vector.
data Vector a = Vector a a
    deriving (Eq, Read, Show, Generic)

instance Hashable a => Hashable (Vector a)

instance Num a => Num (Vector a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (Vector a) where
    recip = fmap recip
    fromRational = pure . fromRational

instance Real a => Ord (Vector a) where
    compare = compare `on` (dot <*> \x->x)

instance Applicative Vector where
    pure x = Vector x x 
    (Vector fx fy) <*> (Vector x y) = Vector (fx x) (fy y)

instance Functor Vector where fmap = liftA

instance Foldable Vector where
    foldr f b (Vector x y) = Foldable.foldr f b [x, y]

instance Traversable Vector where
    sequenceA (Vector x y) = Vector <$> x <*> y

instance (Real i, Ix i) => Ix (Vector i) where
    range (Vector l1 l2, Vector h1 h2) = uncurry Vector <$> range ((l1, l2), (h1, h2))
    index (Vector l1 l2, Vector h1 h2) (Vector x1 x2) = Data.Ix.index ((l1, l2), (h1, h2)) (x1, x2)
    inRange (Vector l1 l2, Vector h1 h2) (Vector x1 x2) = inRange ((l1, l2), (h1, h2)) (x1, x2)

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = Traversable.sequence $ pure arbitrary

-- | Interact with a single component from a vector
component :: Dimension -> Lens' (Vector a) a
component d = lens get make where
  get = fromJust
      . Foldable.foldr (\(d', x) a -> a <|> (guard (d == d') >> Just x)) Nothing
      . liftA2 (,) dimensions
  make v a = (\(a', d') -> if d == d' then a else a') <$> liftA2 (,) v dimensions

-- | Construct a vector with one element different from the others
singleV :: a -> Dimension -> a -> Vector a
singleV zero d x = set (component d) x $ pure zero

-- | Construct a vector from a Dimension-value mapping
vector :: Foldable t => a -> t (Dimension, a) -> Vector a
vector = Foldable.foldr (uncurry $ set . component) . pure

-- | Magnitude of a vector
magnitude :: Floating a => Vector a -> a
magnitude v = sqrt $ dot v v

-- | Redcue a vector's magnitude to 1
normalize :: (Eq a, Floating a) => Vector a -> Vector a
normalize 0 = 0
normalize v = v <&> (/ magnitude v)

-- | Dot product
dot :: Num a => Vector a -> Vector a -> a
dot x y = Foldable.sum $ x * y
