module Stream (test) where

import Prelewd

import Impure

import Data.Char
import Data.Tuple
import Storage.List

import Test.HUnit hiding (Test, test)
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import Util.Stream

test :: Test
test = $(testGroupGenerator)

fromStream :: Stream a b -> [a] -> [b]
fromStream s (x:xs) = uncurry (:) $ s $< x <&> (`fromStream` xs)
fromStream _ _ = []

dscanl :: (a -> b -> a) -> a -> [b] -> [a]
dscanl f b l = tail (scanl f b l) <?> error "scanl returned empty"

case_fib :: Assertion
case_fib = [1, 1, 2, 3, 5, 8, 13, 21, 34, 55] @=? take 10 (fromStream fib $ repeat ())
    where
        fib :: Stream () Integer
        fib = fst <$> updater (\() (x, y) -> (y, x + y)) (0, 1)

prop_compose :: [Integer] -> Bool
prop_compose vals = dscanl (+) 0 vals == fromStream str vals
    where
        str = id >>> (updater (+) 0 >>> id)

prop_map :: (Integer, [Integer]) -> Bool
prop_map (n, is) = map (*n) is == fromStream ((*n) <$> id) is

prop_conjunct :: [Integer] -> Bool
prop_conjunct is = zip (dscanl (+) 0 is) is == fromStream (updater (+) 0 &&& id) is
