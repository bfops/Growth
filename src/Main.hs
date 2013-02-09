{-# LANGUAGE NoImplicitPrelude
           , TemplateHaskell
           , TupleSections
           #-}
-- | Main module, entry point
module Main (main) where

import Prelewd

import IO

import Control.Stream
import Data.Tuple
import Storage.Id
import Storage.Map (lookup)
import Storage.Set

import Wrappers.Events
import Wrappers.GLFW
import qualified Wrappers.OpenGL as OGL

import Game.Input
import Game.State
import Game.Vector
import Physics.Types

import Config

import Main.Graphics

-- | Loop with an iterator.. forever
loop :: Monad m => (a -> m a) -> a -> m a
loop f x = f x >>= loop f

fromMoveEvent :: Event -> Maybe (OGL.Position)
fromMoveEvent (MouseMoveEvent p) = Just p
fromMoveEvent _ = Nothing

-- | Entry point
main :: SystemIO ()
main = runIO $ runGLFW displayOpts (0, 0 :: Integer) title $ do
        initOpenGL
        initEvents
        loop mainLoop $ events
                        >>> identify (id &&& several holdInputs >>> arr resendHeld)
                        >>> several (convertEvents >>> identify game)
                        >>> lift (arr updateGraphics)
    where
        resendHeld (es, pushed) = es <> (toList pushed <&> (\b -> ButtonEvent b Press))

holdInputs :: Stream Id Event (Set Button)
holdInputs = arr Just >>> updater (Id <$$> holdInput) mempty
    where
        holdInput (ButtonEvent b Release) pushed = difference pushed $ set [b]
        holdInput (ButtonEvent b Press) pushed = pushed <> set [b]
        holdInput _ pushed = pushed

convertEvents :: Stream IO Event (Maybe Input)
convertEvents = lift $ convertEvent <$> mousePos <*> id
    where
        convertEvent :: Maybe Position -> Event -> IO (Maybe Input)
        convertEvent _ CloseEvent = mzero
        convertEvent _ (ResizeEvent s) = resize s $> Nothing
        convertEvent _ (ButtonEvent (KeyButton key) Press) = return $ lookup key keymap
        convertEvent mouse (ButtonEvent (MouseButton MouseButton0) Press) = return $ clickAction <$> mouse
        convertEvent _ _ = return Nothing

mousePos :: Stream Id Event (Maybe Position)
mousePos = arr fromMoveEvent >>> map (arr convertPos) >>> latch Nothing
    where
        convertPos :: OGL.Position -> Maybe Position
        convertPos (OGL.Position x y) = mcond (0 <= x && x < fst windowDims && 0 <= y && y < snd windowDims)
                                      $ Vector x (snd windowDims - y) <&> (*) <*> boardDims
                                      <&> div <*> uncurry Vector windowDims

mainLoop :: Stream IO () () -> IO (Stream IO () ())
mainLoop s = (snd <$> s $< ()) <* io (sleep 0.1)
