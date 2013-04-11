{-# LANGUAGE NoImplicitPrelude
           #-}
module Game.Object.Transformation ( Transformation
                                  , transformations
                                  ) where

import Prelewd

import Control.Stream
import Data.Tuple
import Storage.Id

import Physics.Types

import Game.Object.Behaviour
import Game.Object.Heat
import Game.Object.Type

any' :: Foldable t => t (a -> Bool) -> a -> Bool
any' l obj = any ($ obj) l

type Transformation = Stream (Either Object) (Seeds, Heat) ()

infix 1 =>>
(=>>) :: Behaviour -> Object -> Transformation
b =>> obj = Stream $ \seeds -> (=>> obj) <$$> (b $< seeds) <&> Right <?> Left obj

solid, transparent :: Maybe Object -> Bool
solid = any' [grass, rock, ice, dirt, snow]
transparent = any' [water, air]

waterFlow :: Stream Id (Seeds, Heat) (Maybe Flow)
waterFlow = arr $ \(seeds, _) -> considerGround (down seeds) <$> resultFlow seeds
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

-- | The conditions for Lava to convert to Rock
lavaToRock :: Behaviour
lavaToRock = sequence_ [hydrophilic, mix Air]

snowFall :: Transformation
snowFall = wait (arr $ fst >>> snow . up) =>> Snow

transformations :: Object -> [Transformation]

transformations Fire = [magmify, waterThrough]
transformations Grass = [magmify, mix Fire =>> Fire, mix (Lava False) =>> Fire]

transformations (Water s) = [magmify, heat (initHeat Ice) =>> Ice, lift flow, snowFall]
    where
        flow = if s == Nothing
               then arr $ \_-> Right ()
               else waterFlow <&> \m -> diff . Water <$> m <?> Left Air

        diff obj = iff (obj == Water s) (Right ()) (Left obj)

transformations (Lava b) = [wait (arr volcano) =>> Lava True, lavaToRock =>> Rock]
                        <> mcond b despawn
    where
        volcano (seeds, _) = all lava ([left, right, down] <&> ($ seeds)) && (rock (up seeds) || dirt (up seeds))

        despawn = wait (arr $ fst >>> all (all molten)) =>> Lava False

        molten obj = lava obj || rock obj

transformations Rock =
        [ count 32 (neighbour water) `except` wait (arr $ fst >>> any (any lava)) =>> Dirt
        , volcano
        , heat (initHeat $ Lava False) `except` lavaToRock =>> Lava False
        , magmify
        ]
    where
        volcano = wait (arr $ fst >>> (Just (Lava True) ==) . down) =>> Lava True

transformations Dirt = [mix (Lava False) =>> Lava False, magmify]

transformations Air = [magmify, waterThrough, count 32 (neighbour dirt) =>> Grass, snowFall]

transformations Ice = [magmify, heat (initHeat $ Water Nothing) =>> Water Nothing]

transformations Snow =
        [ wait (arr $ fst >>> transparent . down) =>> Air
        , heat (initHeat $ Water Nothing) =>> Water Nothing
        , heat (initHeat Ice) =>> Ice
        ]
