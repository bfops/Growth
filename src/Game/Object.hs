{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
-- | Basic game object type, and associated functions
module Game.Object ( object
                   , Update
                   , Board
                   ) where

import Summit.Control.Stream
import Summit.Data.Array
import Summit.Data.Id
import Summit.Prelewd

import Data.Tuple

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
