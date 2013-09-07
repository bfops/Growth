{-# LANGUAGE NoImplicitPrelude
           #-}
module Main.Graphics ( initOpenGL
                     , updateGraphics
                     , resize
                     ) where

import Summit.IO
import Summit.Prelewd

import Game.Render
import Game.State
import Game.Vector
import qualified Physics.Types as Game

import Wrappers.Events
import Wrappers.GLFW
import Wrappers.OpenGL hiding (position)

import Config

-- | Initialize the OpenGL context
initOpenGL :: IO ()
initOpenGL = io $ do
        shadeModel $= Smooth
        clearDepth $= 1
        depthFunc $= Just Less
        hint PerspectiveCorrection $= Nicest

-- | Draw one frame of the game state
drawFrame :: Game.Position
          -> GameState  -- ^ State to draw
          -> IO ()
drawFrame origin g = do
        -- Clear the screen
        io $ clear [ ColorBuffer, DepthBuffer ]
        -- Reset the view
        io loadIdentity
        io $ translate $ vector3 $ (-2) * (realToFrac <$> origin) / screenDims
        
        draw g

        -- Write it all to the buffer
        io flush
    where
        vector3 :: Vector GLdouble -> Vector3 GLdouble
        vector3 (Vector x y) = Vector3 x y 0

-- | Resize OpenGL view
resize :: Size -> IO ()
resize s = io $ do
        viewport $= (Position 0 0, s)
    
        matrixMode $= Projection
        loadIdentity
    
        matrixMode $= Modelview 0
        loadIdentity

-- | One iteration of graphics
updateGraphics :: Game.Position -> GameState -> IO ()
updateGraphics origin g = drawFrame origin g >> io swapBuffers
