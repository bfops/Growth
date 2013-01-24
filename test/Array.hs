module Array (test) where

import Prelewd

import Impure

import Data.Char
import Data.Tuple
import Storage.Array
import Storage.List

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import Util.Id

test :: Test
test = $(testGroupGenerator)

prop_assign :: Array Integer Integer -> Bool
prop_assign a = a == listArray (bounds a) (elems a)

prop_ordering :: Array Integer Integer -> Bool
prop_ordering a = assocs a == zip (indices a) (elems a)
