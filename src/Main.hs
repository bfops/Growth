{-# LANGUAGE NoImplicitPrelude
           , TemplateHaskell
           , TupleSections
           #-}
-- | Main module, entry point
module Main (main) where

import Prelewd

import Impure
import IO

import Data.Tuple
import Storage.Map

import Wrappers.Events
import Wrappers.GLFW
import qualified Wrappers.OpenGL as OGL

import Config

import Game.Input
import Game.State
import Game.Vector
import Physics.Types
import Util.Id
import Util.Stream

import Main.Graphics

-- | Monad-level if
ifm :: MonadPlus m => m Bool -> m a -> m a
ifm b x = b >>= guard >> x

-- | Loop with an iterator.. forever
loop :: Monad m => (a -> m a) -> a -> m a
loop f x = f x >>= loop f

-- | Is the window open?
isOpen :: EventPoller -> IO Bool
isOpen poll = null <$> poll [CloseEvents]

-- | Lift a Monadic Stream
mstream :: (Functor m, Monad m) => (a -> m b) -> Stream m a b
mstream = lift . arr

-- | Coerce a Stream to take group of inputs. Produces Nothing for empty groups.
extend :: Foldable t => Stream Id a b -> Stream Id (t a) (Maybe b)
extend s = Stream $ Id . map extend . foldr iterate (Nothing, s)
    where
        iterate :: a -> (Maybe b, Stream Id a b) -> (Maybe b, Stream Id a b)
        iterate x (_, Stream f) = map2 Just $ runId $ f x

-- | Entry point
main :: SystemIO ()
main = runIO $ runGLFW displayOpts (0, 0 :: Integer) title $ do
        initOpenGL
        poll <- createEventPoller
        loop (mainLoop poll) $ mstream (\_-> poll [ButtonEvents Nothing Nothing, MouseMoveEvents])
                             >>> identify (batchUpdate >>> latch <&> (<?> error "First update was empty"))
                             >>> mstream (updateGraphics poll)
    where
        -- | Update the GameState by chunks of Events
        batchUpdate :: Stream Id [Event] (Maybe GameState)
        batchUpdate = extend $ convertEvents >>> game

convertEvents :: Stream Id Event (Maybe Input)
convertEvents = (mousePos &&& id) >>> arr (uncurry convertEvent)
    where
        convertEvent :: Maybe Position -> Event -> Maybe Input
        convertEvent _ (ButtonEvent _ Release) = Nothing
        convertEvent _ (ButtonEvent (KeyButton key) _) = lookup key keymap
        convertEvent mouse (ButtonEvent (MouseButton MouseButton0) _) =
                                    Just $ clickAction $ mouse <?> error "No mouse pos"

        convertEvent _ _ = Nothing

mousePos :: Stream Id Event (Maybe Position)
mousePos = mouse >>> map convertPos >>> latch
    where
        mouse :: Stream Id Event (Maybe OGL.Position)
        mouse = arr fromMoveEvent
            where
                fromMoveEvent (MouseMoveEvent p) = Just p
                fromMoveEvent _ = Nothing

        convertPos :: Stream Id OGL.Position Position
        convertPos = arr $ \(OGL.Position x y) -> Vector (toInteger x `div` 50) (toInteger (800-y) `div` 50)

mainLoop :: EventPoller -> Stream IO () () -> IO (Stream IO () ())
mainLoop poll s = ifm (isOpen poll) $ snd <$> s $< ()
