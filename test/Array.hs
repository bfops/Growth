module Array (test) where

import Control.Monad
import Data.Array

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

test :: Test
test = $(testGroupGenerator)

instance (Arbitrary a, Arbitrary i, Ix i, Integral i) => Arbitrary (Array i a) where
  arbitrary = do
      (low, high) <- arbitraryBounds
      as <- replicateM (fromIntegral $ high - low + 1) arbitrary
      return $ listArray (min low high, max low high) as
    where
      arbitraryBounds = do
        l <- arbitrary
        h <- arbitrary
        return (min l h, max l h)

prop_assign :: Array Integer Integer -> Bool
prop_assign a = a == listArray (bounds a) (elems a)

prop_ordering :: Array Integer Integer -> Bool
prop_ordering a = assocs a == zip (indices a) (elems a)
