{-# LANGUAGE LambdaCase #-}
module Data.Conduit.Extra ( awaitJust
                          , exhaustInput
                          , mapFirst
                          , mapSecond
                          , Data.Conduit.Extra.yieldM
                          , zipSourceWith
                          ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Data.Conduit as Conduit
import Data.Conduit.Internal
import Data.Maybe

awaitJust :: Monad m => ConduitM i o m i
awaitJust = fromMaybe (error "awaitJust failed") <$> Conduit.await

yieldM :: Monad m => m a -> Conduit i m a
yieldM m = lift m >>= Conduit.yield

exhaustInput ::
    Monad m =>
    Source m i ->
    ResumableConduit i m o ->
    m (ResumableConduit i m o, [o])
exhaustInput
    = \(ConduitM left0) (ResumableConduit (ConduitM right0) final0) -> goRight final0 left0 right0 []
  where
    goRight final left right result =
        case right of
            HaveOutput next final' o -> goRight (final' >> final) left next (o:result)
            NeedInput rp rc -> goLeft rp rc final left result
            Done () -> return (ResumableConduit (ConduitM right) final, reverse result)
            PipeM mp -> do
                next <- mp
                goRight final left next result
            Leftover next i -> goRight final (HaveOutput left (return ()) i) next result

    goLeft rp rc final left result =
        case left of
            HaveOutput left' final' o -> goRight (final >> final') left' (rp o) result
            NeedInput _ _ -> error "NeedInput Source"
            Done () -> return (ResumableConduit (ConduitM $ NeedInput rp rc) final, reverse result)
            PipeM mp -> do
                next <- mp
                goLeft rp rc final next result
            Leftover _ _ -> error "Leftover Source"

mapFirst :: Monad m => ConduitM i o m r -> ConduitM (i, a) (o, a) m r
mapFirst (ConduitM right) = ConduitM $ go a0 right
  where
    a0 = error "mapFirst producing output before receiving input"

    go :: Monad m => a -> Pipe i i o u m r -> Pipe (i, a) (i, a) (o, a) u m r
    go a = \case
        HaveOutput next final o -> HaveOutput (go a next) final (o, a)
        NeedInput rp rc -> NeedInput (\(i, a') -> go a' $ rp i) (go a . rc)
        Done r -> Done r
        PipeM mp -> PipeM $ mp >>= return . go a
        Leftover next i -> Leftover (go a next) (i, a)

mapSecond :: Monad m => ConduitM i o m r -> ConduitM (a, i) (a, o) m r
mapSecond (ConduitM right) = ConduitM $ go a0 right
  where
    a0 = error "mapSecond producing output before receiving input"

    go :: Monad m => a -> Pipe i i o u m r -> Pipe (a, i) (a, i) (a, o) u m r
    go a = \case
        HaveOutput next final o -> HaveOutput (go a next) final (a, o)
        NeedInput rp rc -> NeedInput (\(a', i) -> go a' $ rp i) (go a . rc)
        Done r -> Done r
        PipeM mp -> PipeM $ mp >>= return . go a
        Leftover next i -> Leftover (go a next) (a, i)

zipSourceWith :: Monad m => (a -> b -> c) -> Source m a -> Source m b -> Source m c
zipSourceWith f sa sb = getZipSource $ f <$> ZipSource sa <*> ZipSource sb
