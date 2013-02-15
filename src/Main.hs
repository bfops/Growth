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
                        >>> several (convertEvents >>> map (identify game))
                        >>> lift (arr $ uncurry updateGraphics)
    where
        resendHeld (es, pushed) = es <> (toList pushed <&> (\b -> ButtonEvent b Press))

mainLoop :: Stream IO () () -> IO (Stream IO () ())
mainLoop s = (snd <$> s $< ()) <* io (sleep 0.1)

holdInputs :: Stream Id Event (Set Button)
holdInputs = arr Just >>> updater (Id <$$> holdInput) mempty
    where
        holdInput (ButtonEvent b Release) pushed = difference pushed $ set [b]
        holdInput (ButtonEvent b Press) pushed = pushed <> set [b]
        holdInput _ pushed = pushed

convertEvents :: Stream IO Event (Position, Maybe Input)
convertEvents = identify origin &&& id
              >>> identify (arr fst) &&& lift (convertEvent <$> mousePos <*> arr snd)
    where
        convertEvent :: Maybe Position -> Event -> IO (Maybe Input)
        convertEvent _ CloseEvent = mzero
        convertEvent _ (ResizeEvent s) = resize s $> Nothing
        convertEvent _ (ButtonEvent (KeyButton key) Press) = return $ lookup key keymap
        convertEvent mouse (ButtonEvent (MouseButton MouseButton0) Press) = return $ clickAction <$> mouse
        convertEvent _ _ = return Nothing

origin :: Stream Id Event Position
origin = arr cameraMoves >>> updater (Id <$$> (+)) 0
    where
        cameraMoves (ButtonEvent (KeyButton KeyLeft) Press) = Just $ Vector (-1) 0
        cameraMoves (ButtonEvent (KeyButton KeyRight) Press) = Just $ Vector 1 0
        cameraMoves (ButtonEvent (KeyButton KeyDown) Press) = Just $ Vector 0 (-1)
        cameraMoves (ButtonEvent (KeyButton KeyUp) Press) = Just $ Vector 0 1
        cameraMoves _ = Nothing

mousePos :: Stream Id (Position, Event) (Maybe Position)
mousePos = map mouseWindowPos >>> arr convertPos
    where
        mouseWindowPos = arr fromMoveEvent >>> latch (error "No initial mouse event")

        convertPos :: (Position, OGL.Position) -> Maybe Position
        convertPos (o, OGL.Position x y) = let
                p = Vector x (snd windowSize - y) <&> (*) <*> screenDims <&> div <*> uncurry Vector windowSize
            in cast (and . liftA2 (\bound i -> i >= 0 && i < bound) boardDims) $ p + o
