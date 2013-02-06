{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Settings are stored in this module
module Config ( viewDist
              , boardDims
              , windowDims
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
import Game.Vector
import Physics.Types
import Wrappers.Events
import Wrappers.GLFW (DisplayOptions (..), defaultDisplayOptions)

-- | Viewing distance of the camera
viewDist :: Int
viewDist = 3

windowDims :: Num a => (a, a)
windowDims = (800, 800)

boardDims :: Num a => Vector a
boardDims = 32

-- | GLFW display options
displayOpts :: DisplayOptions
displayOpts = defaultDisplayOptions
    { displayOptions_width = fst windowDims
    , displayOptions_height = snd windowDims
    , displayOptions_windowIsResizable = False
    }

-- | Title of the game window
title :: Text
title = "Game"

-- | What controls what?
keymap :: Map Key Input
keymap = mapKeys CharKey $ fromList
       [ (' ', Step)
       , ('A', Select Air)
       , ('W', Select $ Water True False)
       , ('R', Select Rock)
       , ('F', Select Fire)
       ]

clickAction :: Position -> Input
clickAction = Place

initBoard :: Board
initBoard = listArray (0, boardDims - 1) (repeat Air)
