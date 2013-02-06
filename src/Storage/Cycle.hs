{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Basic cyclical progression datatype
module Storage.Cycle ( Cycle
                     , cycle
                     ) where

import Prelewd

import Storage.List (span)
import Text.Show

newtype Cycle a = Cycle { list :: [a] }
    deriving(Show)

instance Eq a => Eq (Cycle a) where
    (==) = (==) `on` list

instance Ord a => Ord (Cycle a) where
    compare = compare `on` list

-- | Loop a list into a cycle
cycle :: Ord a => [a] -> Cycle a
cycle l = let (l1, l2) = span (/= minimum l) l
          in Cycle $ l2 <> l1
