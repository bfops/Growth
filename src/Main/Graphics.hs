{-# LANGUAGE NoImplicitPrelude
           #-}
module Main.Graphics ( initOpenGL
                     , updateGraphics
                     ) where

import Prelewd

import IO

import Impure

import Data.Tuple.All
import Storage.List

import Config

import Game.Render
import Game.State
import Wrappers.Events
import Wrappers.GLFW
import Wrappers.OpenGL hiding (position)

-- | Initialize the OpenGL context
initOpenGL :: IO ()
initOpenGL = io $ do
        shadeModel $= Smooth
        clearDepth $= 1
        depthFunc $= Just Less
        hint PerspectiveCorrection $= Nicest

        let glColor = uncurryN Color4 bgColor
        clearColor $= toGLColor (glColor :: Color4 GLubyte)

-- | Draw one frame of the game state
drawFrame :: GameState -- ^ State to draw
          -> IO ()
drawFrame g = do
        -- Clear the screen
        io $ clear [ ColorBuffer, DepthBuffer ]
        -- Reset the view
        io loadIdentity
        --io $ lookAt (Vertex3 0 0 3) (Vertex3 0 0 0) (Vector3 0 1 0)
        io $ ortho (-2.35) 2.35 (-2.35) 2.35 0.1 10
        
        draw g

        -- Write it all to the buffer
        io flush

-- | Resize OpenGL view
resize :: Size -> IO ()
resize s@(Size w h) = io $ do
        viewport $= (Position 0 0, s)
    
        matrixMode $= Projection
        loadIdentity
        perspective 45 (w // h) 0.1 64
    
        matrixMode $= Modelview 0
        loadIdentity
    where
        (//) = (/) `on` realToFrac

-- | Take care of all received resize events
clearResizeEvents :: EventPoller -> IO ()
clearResizeEvents poll = tryResize . last =<< poll [ResizeEvents]
    where
        tryResize = maybe (return ()) resizeEvent

        resizeEvent :: Event -> IO ()
        resizeEvent (ResizeEvent s) = resize s
        resizeEvent _ = error "poll [ResizeEvents] returned an invalid list."

-- | Remove all refresh events from the event poller
clearRefreshEvents :: EventPoller -> IO ()
clearRefreshEvents poll = poll [RefreshEvents] $> ()

-- | One iteration of graphics
updateGraphics :: EventPoller -> GameState -> IO ()
updateGraphics poll g = do
    -- Since we're drawing, all the window refresh events are taken care of
    clearRefreshEvents poll
    clearResizeEvents poll

    drawFrame g

    -- Double buffering
    io swapBuffers
