{-# LANGUAGE NoImplicitPrelude
           , FlexibleInstances
           #-}
-- | Render the game state
module Game.Render ( Drawable (..)
                   ) where

import Prelewd

import IO

import Data.Tuple
import Storage.Array

import Game.Object
import Game.Object.Type
import Game.State
import Game.Vector
import Physics.Types

import Wrappers.OpenGL hiding (Size, Position)

import Config

-- | Convert the game's vector to an OpenGL coordinate
toGLVertex :: Position -> Vertex2 GLdouble
toGLVertex p = let Vector x y = realToFrac <$> 2 * p <&> (/) <*> screenDims
               in on Vertex2 (subtract 1) x y

-- | Things which can be drawn
class Drawable d where
    -- | Render the object to the screen
    draw :: d -> IO ()

instance Drawable GameState where
    draw = sequence_ . elems . mapWithIx (curry draw) . tiles

instance Drawable (Position, WarmObject) where
    draw (p, (o, h)) = drawQuad (objColor o) p
        where
            objColor Fire = orange
            objColor (Water s) = s <&> (\(l, r)-> Color4 0 0.25 (iff (l || r) 0.75 1) 1) <?> blue
            objColor Grass = green
            objColor Rock = grey
            objColor (Lava False) = red
            objColor (Lava True) = Color4 0.6 0.1 0 1
            objColor Air = tempShift cyan
            objColor Dirt = Color4 0.3 0.2 0 1
            objColor Ice = Color4 0.2 0.6 1 1
            objColor Snow = white
            
            tempShift :: Fractional a => Color4 a -> Color4 a
            tempShift (Color4 r g b a) = Color4 (tempShift1 0 1 r) (tempShift1 0 0 g) (tempShift1 1 0 b) a

            tempShift1 t1 t2 k = let bounded = (max (negate maxHeat) $ min maxHeat h)
                                 in k + iff (h > 0) (t2 - k) (k - t1) * ((/) `on` realToFrac) bounded maxHeat

            maxHeat = 32

-- | `draw c o` draws `o` as a quadrilateral, based on its position and size.
drawQuad :: Color4 GLdouble -> Position -> IO ()
drawQuad c p = let
            Vertex2 x y = toGLVertex p
            Vertex2 x' y' = toGLVertex (p + 1)
        in io $ renderPrimitive Quads $ runIO $ drawColored c $
                [ Vertex2 x  y
                , Vertex2 x  y'
                , Vertex2 x' y'
                , Vertex2 x' y
                ]
