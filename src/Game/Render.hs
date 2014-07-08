{-# LANGUAGE FlexibleInstances #-}
-- | Render the game state
module Game.Render ( Drawable (..)
                   , drawQuad
                   ) where

import Control.Applicative
import Control.Lens
import Data.Array
import Game.Object.Type
import Game.State
import Game.Vector
import Physics.Types

import Wrappers.OpenGL hiding (Size, Position)

import Config

-- | Convert the game's vector to an OpenGL coordinate.
-- We render the game world into [0,1) x [0,1).
toGLVertex :: Position -> Vertex2 GLdouble
toGLVertex v =
    let Vector x y = (realToFrac <$> v) / screenDims
    in Vertex2 x y

-- | Things which can be drawn
class Drawable d where
    -- | Render the object to the screen
    draw :: d -> IO ()

mapWithIx :: (Ix i, Eq i) => (i -> a -> b) -> Array i a -> Array i b
mapWithIx f arr = array (bounds arr) [(i, f i a) | (i, a) <- assocs arr]

instance Drawable GameState where
    draw = sequence_ . elems . mapWithIx (curry draw) . tiles

instance Drawable (Position, Tile) where
    draw (p, t)
        = let tileColor = case view tileObject t of
                Fire -> orange
                Water s -> maybe blue flowingColor s
                Grass -> green
                Rock -> grey
                Lava False -> red
                Lava True -> Color4 0.6 0.1 0 1
                Air -> cyan
                Dirt -> Color4 0.3 0.2 0 1
                Ice -> Color4 0.2 0.6 1 1
                Snow -> white
          in drawQuad tileColor p
        where
            flowingColor (l, r) = Color4 0 0.25 (if l || r then 0.75 else 1) 1

-- | `draw c o` draws `o` as a quadrilateral, based on its position and size.
drawQuad :: Color4 GLdouble -> Position -> IO ()
drawQuad c p = let
            Vertex2 x y = toGLVertex p
            Vertex2 x' y' = toGLVertex (p + 1)
    in renderPrimitive Quads $ drawColored c
          [ Vertex2 x  y
          , Vertex2 x  y'
          , Vertex2 x' y'
          , Vertex2 x' y
          ]
