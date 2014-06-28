{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
-- | Main module, entry point
module Main (main) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Conduit as Conduit
import Data.Conduit.List as Conduit
import Data.Foldable as Foldable
import Data.Functor
import Data.HashMap.Strict as HashMap
import Data.OpenUnion
import System.Exit

import Wrappers.Events
import Wrappers.GLFW
import qualified Wrappers.OpenGL as OGL

import Data.Conduit.Extra
import Game.Input
import Game.State
import Game.Vector
import Physics.Types

import Config

import Main.Graphics

-- | Entry point
main :: IO ()
main = void $ runGLFW title Nothing (0, 0 :: Integer) windowSize $ \wnd -> do
        initOpenGL
        initEvents wnd
        putStrLn "starting main loop"
        events
          $= convertEvents
          $= mapSecond game
          $$ Conduit.mapM_ (ioUpdates wnd)
  where
    events :: Source IO (Union '[Event, Tick])
    events
        =  forever (yieldM $ fmap liftUnion <$> popEvents)
        $= Conduit.map (liftUnion Tick :)
        $= Conduit.concat

    ioUpdates wnd (pos, gs) = do
      updateGraphics wnd pos gs
      threadDelay 10000

convertEvents :: Conduit (Union '[Event, Tick]) IO (Position, Union '[Input, Tick])
convertEvents = void (mapAccumM convertUnion (0, Nothing)) =$= Conduit.catMaybes
  where
    convertUnion = convertEvent @> convertTick @> typesExhausted

    convertTick Tick (origin, mouse) = return ((origin, mouse), Just (origin, liftUnion Tick))

    convertEvent CloseEvent _ = do
      putStrLn "exiting"
      exitSuccess
    convertEvent (ResizeEvent s) a = do
      putStrLn "resizing"
      resize s $> (a, Nothing)
    convertEvent (MouseButtonEvent MouseButton'1 MouseButtonState'Pressed _) a@(origin, mouse) = return (a, (origin,) . liftUnion . clickAction <$> mouse)
    convertEvent (MouseMoveEvent pos) (origin, _) = return ((origin, convertPos origin pos), Nothing)
    convertEvent (KeyEvent Key'Left KeyState'Pressed _) (origin, mouse) = return ((origin - Vector 1 0, mouse), Nothing)
    convertEvent (KeyEvent Key'Right KeyState'Pressed _) (origin, mouse) = return ((origin + Vector 1 0, mouse), Nothing)
    convertEvent (KeyEvent Key'Up KeyState'Pressed _) (origin, mouse) = return ((origin + Vector 0 1, mouse), Nothing)
    convertEvent (KeyEvent Key'Down KeyState'Pressed _) (origin, mouse) = return ((origin - Vector 0 1, mouse), Nothing)
    convertEvent (KeyEvent key KeyState'Pressed _) a@(origin, _) = return (a, (origin,) . liftUnion <$> HashMap.lookup key keymap)
    convertEvent (KeyEvent key KeyState'Repeating s) a = convertEvent (KeyEvent key KeyState'Pressed s) a
    convertEvent _ a = return (a, Nothing)

    convertPos :: Position -> OGL.Position -> Maybe Position
    convertPos o (OGL.Position x y) = let
            p0 = div <$> ((*) <$> Vector x (snd windowSize - y) <*> screenDims) <*> uncurry Vector windowSize
            p = fromIntegral <$> p0
        in mfilter (Foldable.and . liftA2 (\bound i -> i >= 0 && i < bound) boardDims) $ Just (p + o)
