{-# LANGUAGE TupleSections #-}
-- | Settings are stored in this module
module Config ( viewDist
              , windowSize
              , boardDims
              , screenDims
              , title
              , keymap
              , clickAction
              , initBoard
              ) where

import Data.Array
import Data.HashMap.Strict as HashMap

import Game.Input
import Game.Object
import Game.Object.Type
import Game.Vector
import Physics.Types
import Wrappers.Events

-- | Viewing distance of the camera
viewDist :: Int
viewDist = 3

windowSize :: Num a => (a, a)
windowSize = (1000, 1000)

-- | Dimensions of the whole board
boardDims :: Num a => Vector a
boardDims = Vector 64 32

-- | Dimensions of the segment of board to show on the screen
screenDims :: Num a => Vector a
screenDims = 32

-- | Title of the game window
title :: String
title = "Growth"

-- | What controls what?
keymap :: HashMap Key Input
keymap = fromList
       [ (Key'Space, Step)
       , (Key'F, Select Fire)
       , (Key'A, Select Air)
       , (Key'R, Select Rock)
       , (Key'S, Select Snow)
       , (Key'L, Select (Lava False))
       , (Key'I, Select Ice)
       , (Key'W, Select (Water Nothing))
       ]

clickAction :: Position -> Input
clickAction = Place

data ShortObject = A | F | R | S
    deriving (Eq)

initBoard :: Board
initBoard
      = fmap Tile
      $ listArray (0, boardDims - 1) (repeat Air)
       // fmap
           (\(b, p) -> (p, obj b))
           (zip objects [Vector x y | y <- reverse [0..31], x <- [0..31]])
   where
       obj A = Air
       obj F = Fire
       obj R = Rock
       obj S = Snow

       objects =
          [ A, A, A, A, A, A, A, A, A, A, A, A, A, S, S, S, S, S, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, F, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, A, A, A, A, A, A, A, A, A, A, A, R, R, R, R, R, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, A, A, A, A, A, A, A, A, A, A, R, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, A, A, A, A, A, A, A, R, R, R, A, A, A, A, A, S, R, R, R, R, R, R, R, A, A, A, A, A, A, A
          , A, A, A, A, A, A, R, R, R, A, A, A, A, A, A, A, S, S, S, A, A, A, A, A, A, R, R, R, A, A, A, A
          , A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, S, S, S, S, S, A, A, A, A, A, A, A, A, R, R, A, A
          , A, A, A, A, A, A, R, A, A, A, A, A, A, A, S, S, S, S, S, S, S, A, A, A, A, A, A, A, A, A, R, A
          , A, A, A, R, R, R, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, R, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, A, R, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, A, A, R, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, A, A, A, R, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, A, A, R, A, A, R, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, A, R, A, A, R, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, A, R, A, R, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, A, R, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, A, A, R, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, A, A, A, R, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A
          , A, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, A, R, R, A
          , R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, A, R, A
          , R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, A, R, A
          , R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, A, R, A
          , A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, F, A, A
          ]
