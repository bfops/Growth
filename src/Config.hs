{-# LANGUAGE NoImplicitPrelude
           #-}
-- | Settings are stored in this module
module Config ( viewDist
              , displayOpts
              , bgColor
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

-- | Background color
bgColor :: Num a => (a, a, a, a)
bgColor = (0, 175, 200, 0)

-- | Title of the game window
title :: Text
title = "Game"

-- | What controls what?
keymap :: Map Key Input
keymap = mapKeys CharKey $ fromList
       [ ('A', Select Fire)
       , ('S', Select Water)
       , ('D', Select Grass)
       ]

clickAction :: Position -> Input
clickAction = Place
