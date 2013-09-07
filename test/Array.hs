module Array (test) where

import Summit.Data.Array
import Summit.Data.Id
import Summit.Data.List
import Summit.Impure
import Summit.Prelewd

import Data.Char
import Data.Tuple

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

test :: Test
test = $(testGroupGenerator)

prop_assign :: Array Integer Integer -> Bool
prop_assign a = a == listArray (bounds a) (elems a)

prop_ordering :: Array Integer Integer -> Bool
prop_ordering a = assocs a == zip (indices a) (elems a)
