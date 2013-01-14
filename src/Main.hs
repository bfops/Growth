{-# LANGUAGE NoImplicitPrelude
           , TemplateHaskell
           , TupleSections
           #-}
-- | Main module, entry point
module Main (main) where

import Prelewd

import Impure
import IO

import Storage.List
import Storage.Map
import Subset.Num
import Template.MemberTransformer

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

-- | Loop with an iterator.. forever
loop :: Monad m => (a -> m a) -> a -> m a
loop f x = f x >>= loop f

-- | Monad-level if
ifm :: MonadPlus m => m Bool -> m a -> m a
ifm b x = b >>= guard >> x

-- | Is the window open?
isOpen :: EventPoller -> IO Bool
isOpen poll = null <$> poll [CloseEvents]

-- | Program state
data State = State { game       :: Stream Id Inputs GameState
                   , mousePos   :: Maybe Position
                   , lastUpdate :: Double
                   , inputs     :: Map Input (Positive Integer)
                   }

$(memberTransformers ''State)

-- | Entry point
main :: SystemIO ()
main = runIO $ runGLFW displayOpts (0, 0 :: Integer) title $ do
        initOpenGL
        poll <- createEventPoller
        quiet $ loop (mainLoop poll) =<< getInitState

-- | Create initial program state
getInitState :: IO State
getInitState = State initGame Nothing <$> io getTime <&> ($ mempty)

mainLoop :: EventPoller -> State -> IO State
mainLoop poll state = ifm (isOpen poll)
                    $ do mouse <- newMouse <$> poll [MouseMoveEvents]
                         input <- getInputs poll mouse
                         let presses = mapMaybe (\(i, b) -> mcond (b == Press) i) input
                             (render, newGame) = runId $ game state $< presses
                         updateGraphics poll render
                         t <- io getTime
                         return $ inputs' (\is -> foldr updateInput is input)
                                $ mousePos' (\_-> mouse)
                                $ game' (\_-> newGame)
                                $ lastUpdate' (\_-> t)
                                $ state
    where
        newMouse :: [Event] -> Maybe Position
        newMouse moves = getMousePos <$> head moves <|> mousePos state

        getMousePos (MouseMoveEvent (OGL.Position x y)) = Vector (toInteger x `div` 50) (toInteger (800-y) `div` 50)
        getMousePos _ = error "MouseMove poll returned other"

-- | Update the input state with a new input event
updateInput :: (Input, ButtonState)
            -> Map Input (Positive Integer)
            -> Map Input (Positive Integer)
updateInput (i, Press) ins = insertWith (\_ -> (+ 1)) i 1 ins
updateInput (i, Release) ins = modify decCount i ins <?> error "Released unpressed input"
    where decCount n = toPos (fromPos n - 1)

-- | Receive all pending input events, and convert them to game input
getInputs :: EventPoller -> Maybe Position -> IO [(Input, ButtonState)]
getInputs poll mmouse = mapMaybe rawToInput <$> poll [ ButtonEvents Nothing Nothing ]
    where
        mouse = mmouse <?> error "Mouse clicked without moving"

        -- Convert an input event to a game input
        rawToInput :: Event -> Maybe (Input, ButtonState)
        rawToInput (ButtonEvent (KeyButton key) s) = lookup key keymap <&> (, s)
        rawToInput (ButtonEvent (MouseButton MouseButton0) s) = Just (clickAction mouse, s)
        rawToInput _ = Nothing
