{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Settings are stored in this module
module Config ( viewDist
              , displayOpts
              , title
              , keymap
              , clickAction
              ) where

import Prelewd

import Storage.Map

import Game.Input
import Game.Object
import Physics.Types
import Wrappers.Events
import Wrappers.GLFW (DisplayOptions (..), defaultDisplayOptions)

-- | Viewing distance of the camera
viewDist :: Int
viewDist = 3

-- | GLFW display options
displayOpts :: DisplayOptions
displayOpts = defaultDisplayOptions
    { displayOptions_width = 800
    , displayOptions_height = 800
    , displayOptions_windowIsResizable = False
    }

-- | Title of the game window
title :: Text
title = "Game"

-- | What controls what?
keymap :: Map Key Input
keymap = mapKeys CharKey $ fromList
       [ (' ', Step)
       , ('A', Select $ Water False)
       , ('S', Select $ Lava False)
       ]

clickAction :: Position -> Input
clickAction = Place
