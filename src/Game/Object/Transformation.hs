{-# LANGUAGE NoImplicitPrelude
           #-}
module Game.Object.Transformation ( transformations
                                  ) where

import Prelewd

import Control.Stream
import Storage.Id

import Game.Object.Type
import Game.Object.Behaviour

type Transformation = Stream (Either Object) Seeds ()

infix 1 =>>
(=>>) :: Behaviour -> Object -> Transformation
b =>> obj = Stream $ \seeds -> (=>> obj) <$$> (b $< seeds) <&> Right <?> Left obj

waterFlow :: Stream Id Seeds (Maybe Flow)
waterFlow = arr $ \seeds -> considerGround (down seeds) <$> resultFlow seeds
    where
        considerGround ground flow = Just $ iff (solid ground) flow (False, False)

        resultFlow seeds = if water $ up seeds
                           then Just (True, True)
                           else let l = rightWater $ left seeds
                                    r = leftWater $ right seeds
                                in mcond (l || r) (not l, not r)

-- | Incorporate flowing Water
hydrophilic :: Behaviour
hydrophilic = lift $ waterFlow >>> arr (\m -> (\_-> Nothing) <$> m <?> Just ())

-- | Water will flow through these tiles
waterThrough :: Transformation
waterThrough = lift $ waterFlow >>> arr (\m -> Left . Water <$> m <?> Right ())

magmify :: Transformation
magmify = mix (Lava True) =>> Lava False

lavaToRock :: Behaviour
lavaToRock = sequence_ [hydrophilic, mix Air]

snowFall :: Transformation
snowFall = wait (arr $ snow . up) =>> Snow

transformations :: Object -> [Transformation]

transformations Fire = [magmify, waterThrough]
transformations Grass = [magmify, mix Fire =>> Fire, mix (Lava False) =>> Fire]

transformations (Water s) = [magmify, heat (-16) =>> Ice, lift flow, snowFall]
    where
        flow = if s == Nothing
               then arr $ \_-> Right ()
               else waterFlow <&> \m -> diff . Water <$> m <?> Left Air

        diff obj = iff (obj == Water s) (Right ()) (Left obj)

transformations (Lava b) = [wait (arr volcano) =>> Lava True, lavaToRock =>> Rock]
                        <> mcond b despawn
    where
        volcano seeds = all lava ([left, right, down] <&> ($ seeds)) && (rock (up seeds) || dirt (up seeds))

        despawn = wait (arr $ all $ all $ lava <&> (||) <*> rock) =>> Lava False

transformations Rock =
        [ count 32 (neighbour water) `except` wait (arr $ any $ any lava) =>> Dirt
        , volcano
        , heat 8 `except` lavaToRock =>> Lava False
        , magmify
        ]
    where
        volcano = wait (arr $ (Just (Lava True) ==) . down) =>> Lava True

transformations Dirt = [mix (Lava False) =>> Lava False, magmify]

transformations Air = [magmify, waterThrough, count 32 (neighbour dirt) =>> Grass, snowFall]

transformations Ice = [magmify, heat 2 =>> Water Nothing]

transformations Snow =
        [ heat 1 =>> Water Nothing
        , wait (arr $ not . solid . down) =>> Air
        , count 32 (arr snowToIce) =>> Ice
        ]
    where
        snowToIce s = iff (all (all solid) s) (neighbour snow s) 0
