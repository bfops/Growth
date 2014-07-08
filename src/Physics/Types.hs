{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Functions and data structures for dealing with physical game aspects
module Physics.Types ( PhysicsValue
                     , Scalar
                     , Time
                     , Distance
                     , Heat
                     , Temperature
                     , HeatCapacity
                     , HeatResistence
                     , Size
                     , Position
                     ) where

import Data.Fixed

import Game.Vector

-- | Root value type
type PhysicsValue = Integer

-- | Unitless value
type Scalar = PhysicsValue

-- | Against which rates are measured
type Time = Milli
-- | Measure of space
type Distance = PhysicsValue
-- | Measure of stored heat energy
type Heat = Milli
type Temperature = Heat
-- | dE/dT
type HeatCapacity = Micro
-- | scalar factor inversely proportional to how easily heat is transferred.
type HeatResistence = Integer

-- | Dimensions of an object
type Size = Vector Distance
-- | Location in space
type Position = Vector Distance
