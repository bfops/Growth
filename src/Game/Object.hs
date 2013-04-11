{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
module Game.Object ( object
                   , WarmObject
                   , Neighbours
                   , Update
                   , Board
                   ) where

import Prelewd

import Control.Stream
import Data.Tuple
import Storage.Array
import Storage.Id
import Storage.Pair

import Physics.Types

import Game.Object.Heat
import Game.Object.Type
import Game.Object.Transformation
import Game.Vector

type WarmObject = (Object, Heat)
type Neighbours = Vector (Pair (Maybe WarmObject))
type Update = Stream Id Neighbours (Object, Heat)
type Board = Array Position WarmObject

transformation :: Object -> Transformation
transformation obj = sequence_ $ transformations obj

object :: Object -> Update
object initObj = updater ( (arr $ fst >>> seeds) &&& barr updateHeat
                       >>> updateObject initObj &&& arr snd
                         )
                 (initObj, initHeat initObj)
    where
        seeds neighbours = map fst <$$> neighbours

updateObject :: Object -> Stream Id (Seeds, Heat) Object
updateObject initObj = loop (barr newObject) (transformation initObj) >>> latch initObj
    where
        newObject info s = either accum ((Nothing,) . snd) (s $< info)
        accum obj = (Just obj, transformation obj)

updateHeat :: Neighbours -> WarmObject -> Heat
updateHeat ns (obj, h) = h + sum (mapMaybe (map dT) $ concatMap toList ns)
    where
        dT (obj2, h2) = ((*) `on` realToFrac . capacity) obj obj2 * ((-) `on` realToFrac) h2 h
