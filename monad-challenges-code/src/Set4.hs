{-# language NoImplicitPrelude #-}
module Set4 where

import Prelude (Monad (..), Maybe)
import MCPrelude
--import Set2

--data Maybe a = Nothing | Just a

--class Monad m where
--  return :: a -> m a
--  (>>=) :: m a -> (a -> m b) -> m b
--  (>>=) :: m a -> (a -> m b) -> m b
--  (>>=) = (>>=)

--instance Monad Maybe where
--  return = Just
--  (>>=) Nothing _  = Nothing
--  (>>=) (Just x) f = f x

--instance Monad [] where
--  return = \x -> (x:[])
--  (>>=) [] _     = []
--  (>>=) (x:xs) f = f x ++ (>>=) xs f

newtype Gen t = Gen { getGen :: Seed -> (t, Seed) }

--instance Monad Gen where
--  return x  = Gen (\s -> (x, s))
--  (>>=) gx f = Gen (\s -> let (y, t) = (getGen gx) s
--                 in getGen (f y) t)

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
--liftM2 f ma mb = ma >>= \x -> mb >>= \y -> return (f x y)
liftM2 f ma mb = do
  x <- ma
  y <- mb
  return (f x y)


fmap :: Monad m => (a -> b) -> m a -> m b
fmap f ma = (>>=) ma (return . f)

sequence :: Monad m => [m a] -> m [a]
--sequence mas = foldr (\ma mas -> (fmap (:) ma >>= \f -> fmap f mas)) (return []) mas
sequence mas =
  let f ma mas = do
        a <- ma
        as <- mas
        return (a:as)
  in
        foldr f (return []) mas

--(>>=) :: Monad m => m a -> (a -> m b) -> m b
--(>>=) = (>>=) 

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=) 

join :: Monad m => m (m a) -> m a
join mma = (>>=) mma id

(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = liftM2 ($)

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = liftM2 f ma mb <*> mc

-- monad
--genTwo :: Gen a -> (a -> Gen b) -> Gen b
--link :: Maybe a -> (a -> Maybe b) -> Maybe b
--chain :: (a -> Maybe b) -> Maybe a -> Maybe b
--(>>=) :: m a -> (a -> m b) -> m b
--(=<<) :: (a -> m b) -> m a -> m b
--
---- the other one
--mkGen :: a -> Gen a
--mkMaybe :: a -> Maybe a
--return :: a -> m a
--
---- can be made out of above 
--generalA :: (a -> b) -> Gen a -> Gen b
--transMaybe :: (a -> b) -> Maybe a -> Maybe b
--
--generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
--yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
--combStep :: [a -> b] -> [a] -> [b]
--allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
--allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
--allCombs4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
--generalPair :: Gen a -> Gen b -> Gen (a, b)


