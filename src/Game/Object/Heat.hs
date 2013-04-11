{-# LANGUAGE NoImplicitPrelude
           #-}
module Game.Object.Heat ( capacity
                        , initHeat
                        ) where

import Prelewd

import Physics.Types

import Game.Object.Type

initHeat :: Object -> Heat
initHeat Fire = 32
initHeat (Lava b) = if' b (2 *) 128
initHeat Ice = -32
initHeat Snow = -4
initHeat _ = 0

capacity :: Object -> HeatCapacity
capacity Fire = 0.6
capacity (Lava _) = 0.2
capacity Grass = 0.4
capacity (Water _) = 0.6
capacity Air = 0.8
capacity Rock = 0.1
capacity Dirt = 0.2
capacity Ice = 0.4
capacity Snow = 0.5
