{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
-- | Main module, entry point
module Main (main) where

import Summit.Impure
import Summit.IO
import Summit.Control.Stream
import Summit.Prelewd
import Summit.Data.Id
import Summit.Data.List
import Summit.Data.Map (lookup)
import Summit.Data.Set

import Data.Tuple

import Wrappers.Events
import Wrappers.GLFW
import qualified Wrappers.OpenGL as OGL

import Game.Input
import Game.State
import Game.Vector
import Physics.Types

import Config

import Main.Graphics

fromMoveEvent :: Event -> Maybe OGL.Position
fromMoveEvent (MouseMoveEvent p) = Just p
fromMoveEvent _ = Nothing

severalEvents :: (Applicative m, Monad m) => Stream m a b -> Stream m [a] b
severalEvents s = map s >>> identify (arr last >>> latch (error "No initial event!"))

-- | Entry point
main :: SystemIO ()
main = runIO $ runGLFW displayOpts (0, 0 :: Integer) title $ do
        initOpenGL
        initEvents
        iterateM_ (map snd . ($< ())) $ events
                                    >>> identify (id &&& severalEvents holdInputs >>> arr resendHeld)
                                    >>> severalEvents (convertEvents >>> map (identify game))
                                    >>> lift (barr updateGraphics)
                                    >>> lift (arr $ \_-> io $ sleep 0.1)
    where
        resendHeld (es, pushed) = es <> (toList pushed <&> (\b -> ButtonEvent b Press))

holdInputs :: Stream Id Event (Set Button)
holdInputs = folds (barr holdInput) mempty
    where
        holdInput (ButtonEvent b Release) pushed = pushed \\ set [b]
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
origin = arr cameraMoves >>> folds (barr (+)) 0
    where
        cameraMoves (ButtonEvent (KeyButton KeyLeft) Press) = Vector (-1) 0
        cameraMoves (ButtonEvent (KeyButton KeyRight) Press) = Vector 1 0
        cameraMoves (ButtonEvent (KeyButton KeyDown) Press) = Vector 0 (-1)
        cameraMoves (ButtonEvent (KeyButton KeyUp) Press) = Vector 0 1
        cameraMoves _ = 0

mousePos :: Stream Id (Position, Event) (Maybe Position)
mousePos = map mouseWindowPos >>> arr convertPos
    where
        mouseWindowPos = arr fromMoveEvent >>> latch (error "No initial mouse event")

        convertPos :: (Position, OGL.Position) -> Maybe Position
        convertPos (o, OGL.Position x y) = let
                p = Vector x (snd windowSize - y) <&> (*) <*> screenDims <&> div <*> uncurry Vector windowSize
            in cast (and . liftA2 (\bound i -> i >= 0 && i < bound) boardDims) $ p + o
