module Stream (test) where

import Prelewd

import Impure

import Data.Char
import Data.Tuple
import Storage.List

import Test.HUnit hiding (Test, test)
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import Util.Id
import Util.Stream

idm :: (Functor m, Monad m) => Stream m (m a) a
idm = Stream $ map (,idm)

test :: Test
test = $(testGroupGenerator)

fromStream :: (Functor m, Monad m) => Stream m a b -> [a] -> m [b]
fromStream s (x:xs) = do
            (r, s') <- s $< x
            (r:) <$> fromStream s' xs
fromStream _ _ = return []

whileJust :: Stream Maybe a b -> [a] -> [b]
whileJust s l = recurse <$> ((s $<) =<< head l) <*> tail l <?> []
    where
        recurse (b, s') as = b : whileJust s' as

dscanl :: (a -> b -> a) -> a -> [b] -> [a]
dscanl f b l = tail (scanl f b l) <?> error "scanl returned empty"

case_fib :: Assertion
case_fib = [1, 1, 2, 3, 5, 8, 13, 21, 34, 55] @=? take 10 (runId $ fromStream fib $ repeat $ Just ())
    where
        fib = fst <$> updater (\() (x, y) -> (y, x + y)) (0, 1)

prop_compose :: [Integer] -> Bool
prop_compose vals = dscanl (+) 0 vals == runId (fromStream str $ Just <$> vals)
    where
        str = id >>> (updater (+) 0 >>> id)

prop_map :: (Integer, [Integer]) -> Bool
prop_map (n, ns) = map (*n) ns == runId (fromStream ((*n) <$> id) ns)

prop_conjunct :: [Integer] -> Bool
prop_conjunct ns = zip (dscanl (+) 0 ns) (dscanl (flip (-)) 0 ns)
                 == runId (fromStream (updater (+) 0 &&& updater (-) 0) $ Just <$> ns)

prop_monad :: [Integer] -> Bool
prop_monad ns = ns == whileJust (heads ns) (repeat ())
    where
        heads :: [Integer] -> Stream Maybe () Integer
        heads l = Stream $ \_ -> head l <&> (,) <*> (heads <$> tail l)

prop_maybe :: [Maybe Integer] -> Bool
prop_maybe ns = ((+1) <$$> ns) == runId (fromStream (map $ arr (+1)) ns)
