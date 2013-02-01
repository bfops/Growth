{-# LANGUAGE NoImplicitPrelude
           #-}
module Main.Graphics ( initOpenGL
                     , updateGraphics
                     , resize
                     ) where

import Prelewd

import IO

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

-- | One iteration of graphics
updateGraphics :: GameState -> IO ()
updateGraphics g = drawFrame g >> io swapBuffers
