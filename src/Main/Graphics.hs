module Main.Graphics ( initOpenGL
                     , updateGraphics
                     , resize
                     ) where

import Control.Applicative
import Data.Function
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
initOpenGL = do
    shadeModel $= Smooth
    clearDepth $= 100
    depthFunc $= Just Less
    hint PerspectiveCorrection $= Nicest
    clearColor $= Color4 0.25 0 0 1

    resize $ uncurry (Size `on` fromInteger) windowSize

-- | Resize OpenGL view
resize :: Size -> IO ()
resize s = do
    viewport $= (Position 0 0, s)

    matrixMode $= Projection
    loadIdentity
    -- Sit at (0, 0, -2), and look at the
    -- (0, 0, 0) to (0, 1, 0) area.
    let aspect = let (w, h) = windowSize in w / h
    frustum 0 (1/2) 0 (1/2/aspect) 1 4
    translate (Vector3 0 0 (-2) :: Vector3 GLdouble)

    matrixMode $= Modelview 0
    loadIdentity

-- | One iteration of graphics
updateGraphics :: Window -> Game.Position -> GameState -> IO ()
updateGraphics wnd origin g = do
    -- Clear the screen.
    clear [ ColorBuffer, DepthBuffer ]
    -- Reset the view.
    loadIdentity
    translate $ vector3 $ negate $ (realToFrac <$> origin) / screenDims

    draw g

    swapBuffers wnd
    flush
  where
    vector3 :: Vector GLdouble -> Vector3 GLdouble
    vector3 (Vector x y) = Vector3 x y 0
