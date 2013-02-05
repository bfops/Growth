{-# LANGUAGE NoImplicitPrelude
           , TemplateHaskell
           , TupleSections
           #-}
-- | Main module, entry point
module Main (main) where

import Prelewd

import Impure
import IO

import Control.Stream
import Data.Tuple
import Storage.Id
import Storage.Map

import Wrappers.Events
import Wrappers.GLFW
import qualified Wrappers.OpenGL as OGL

import Config

import Game.State
import Game.Vector
import Physics.Types

import Main.Graphics

-- | Loop with an iterator.. forever
loop :: Monad m => (a -> m a) -> a -> m a
loop f x = f x >>= loop f

fromMoveEvent :: Event -> Maybe (OGL.Position)
fromMoveEvent (MouseMoveEvent p) = Just p
fromMoveEvent _ = Nothing

-- | Coerce a Stream to take chunks of inputs. Produces Nothing for empty groups.
inChunks :: (Functor m, Monad m, Foldable t) => Stream m a b -> Stream m (t a) b
inChunks s0 = updateSeveral s0 >>> identify (latch $ error "First update empty")
    where
        updateSeveral s = Stream $ \l -> updateSeveral <$$> foldrM iterate (Nothing, s) l
        iterate x (_, Stream f) = map2 Just <$> f x

-- | Entry point
main :: SystemIO ()
main = runIO $ runGLFW displayOpts (0, 0 :: Integer) title $ do
        initOpenGL
        initEvents
        loop mainLoop $ events >>> inChunks (convertEvents >>> identify game) >>> lift (arr updateGraphics)

convertEvents :: Stream IO Event (Maybe Input)
convertEvents = lift $ convertEvent <$> mousePos <*> id
    where
        convertEvent :: Position -> Event -> IO (Maybe Input)
        convertEvent _ CloseEvent = mzero
        convertEvent _ (ResizeEvent s) = resize s $> Nothing
        convertEvent _ (ButtonEvent (KeyButton key) Press) = return $ lookup key keymap
        convertEvent mouse (ButtonEvent (MouseButton MouseButton0) Press) = return $ Just $ clickAction mouse
        convertEvent _ _ = return Nothing

mousePos :: Stream Id Event Position
mousePos = arr fromMoveEvent
         >>> map (arr convertPos)
         -- GLFW should send a mouse event as the first event
         >>> latch (error "No mouse pos")
    where
        convertPos :: OGL.Position -> Position
        convertPos (OGL.Position x y) = Vector (toInteger x `div` 25) (toInteger (800-y) `div` 25)

mainLoop :: Stream IO () () -> IO (Stream IO () ())
mainLoop s = snd <$> s $< ()
