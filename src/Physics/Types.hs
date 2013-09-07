{-# LANGUAGE NoImplicitPrelude
           , FlexibleInstances
           , MultiParamTypeClasses
           #-}
-- | Functions and data structures for dealing with physical game aspects
module Physics.Types ( PhysicsValue
                     , Scalar
                     , Time
                     , Distance
                     , Size
                     , Position
                     ) where

import Summit.Prelewd
import Summit.Subset.Num

import Data.Fixed

import Game.Vector

-- | Root value type
type PhysicsValue = Integer

-- | Unitless value
type Scalar = PhysicsValue

-- | Against which rates are measured
type Time = Nonnegative Milli
-- | Measure of space
type Distance = PhysicsValue

-- | Dimensions of an object
type Size = Vector (Positive Distance)
-- | Location in space
type Position = Vector Distance
