{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
-- | Settings are stored in this module
module Config ( viewDist
              , windowSize
              , boardDims
              , screenDims
              , displayOpts
              , title
              , keymap
              , clickAction
              , initBoard
              ) where

import Prelewd

import Data.Tuple
import Storage.Array
import Storage.List
import Storage.Map

import Game.Input
import Game.Object
import Game.Object.Heat
import Game.Object.Type
import Game.Vector
import Physics.Types
import Wrappers.Events
import Wrappers.GLFW (DisplayOptions (..), defaultDisplayOptions)

-- | Viewing distance of the camera
viewDist :: Int
viewDist = 3

windowSize :: Num a => (a, a)
windowSize = (1000, 1000)

-- | Dimensions of the whole board
boardDims :: Num a => Vector a
boardDims = 3

-- | Dimensions of the segment of board to show on the screen
screenDims :: Num a => Vector a
screenDims = 3

-- | GLFW display options
displayOpts :: DisplayOptions
displayOpts = defaultDisplayOptions
    { displayOptions_width = fst windowSize
    , displayOptions_height = snd windowSize
    , displayOptions_windowIsResizable = False
    }

-- | Title of the game window
title :: Text
title = "Growth"

-- | What controls what?
keymap :: Map Key Input
keymap = mapKeys CharKey $ fromList
       [ (' ', Step)
       , ('F', Select Fire)
       , ('A', Select Air)
       , ('R', Select Rock)
       , ('S', Select Snow)
       ]

clickAction :: Position -> Input
clickAction = Place

initBoard :: Board
initBoard = (id &&& initHeat)
        <$> listArray (0, boardDims - 1) (repeat Air)
         // [(0, Snow)]
