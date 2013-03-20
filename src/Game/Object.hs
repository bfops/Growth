{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
-- | Basic game object type, and associated functions
module Game.Object ( object
                   , Update
                   , Board
                   ) where

import Prelewd

import Control.Stream
import Data.Tuple
import Storage.Array
import Storage.Id

import Physics.Types

import Game.Object.Type
import Game.Object.Transformation

type Update = Stream Id Seeds Object
type Board = Array Position Object

object :: Object -> Update
object initObj = loop (barr updateObject) (behaviour initObj) >>> latch initObj
    where
        accum obj = (Just obj, behaviour obj)
        behaviour obj = sequence_ $ transformations obj
        updateObject seeds s = either accum ((Nothing,) . snd) (s $< seeds)
