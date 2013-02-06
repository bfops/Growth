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

-- | Coerce a Stream to take chunks of inputs.
inChunks :: (Functor m, Monad m, Foldable t) => Stream m a b -> Stream m (t a) b
inChunks s0 = updateSeveral s0 >>> identify (latch $ error "First update empty")
    where
        updateSeveral s = Stream $ \l -> updateSeveral <$$> foldlM iterate (Nothing, s) l
        iterate (_, Stream f) x = map2 Just <$> f x

-- | Entry point
main :: SystemIO ()
main = runIO $ runGLFW displayOpts (0, 0 :: Integer) title $ do
        initOpenGL
        initEvents
        loop mainLoop $ events
                        >>> identify (id &&& inChunks holdInputs >>> arr resendHeld)
                        >>> inChunks (convertEvents >>> identify game)
                        >>> lift (arr updateGraphics)
    where
        resendHeld (es, pushed) = es <> (toList pushed <&> (\b -> ButtonEvent b Press))

holdInputs :: Stream Id Event (Set Button)
holdInputs = arr Just >>> updater holdInput mempty
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
        convertPos (OGL.Position x y) = mcond (0 <= x && x < 800 && 0 <= y && y < 800)
                                      $ Vector (toInteger x `div` 25) (toInteger (800-y) `div` 25)

mainLoop :: Stream IO () () -> IO (Stream IO () ())
mainLoop s = (snd <$> s $< ()) <* io (sleep 0.1)
