{-# language NoImplicitPrelude #-}
module Set3 where

import Prelude (foldMap)
import MCPrelude
import Set4

allPairs :: [a] -> [b] -> [(a,b)]
allPairs = allCombs (,)

data Card = Card {rank :: Int, suit :: String} 
instance Show Card where
        show c = show (rank c) ++ (suit c)

allCards :: [Int] -> [String] -> [Card]
allCards = allCombs Card

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs = liftM2
--allCombs f xs ys = foldMap (\x -> map (f x) ys) xs

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 = liftM3
--allCombs3 f xs ys zs = foldMap (\x -> foldMap (\y -> map (f x y) zs) ys) xs

combStep :: [a -> b] -> [a] -> [b]
combStep fs xs = foldMap (\f -> map f xs) fs

allPairs2 xs = combStep (map (,) xs)
allCards2 xs = combStep (map Card xs)
allCombs2 f xs = combStep (map f xs)
allCombs32 f xs ys = combStep $ combStep (map f xs) ys

allCombs4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
allCombs4 f as bs cs = combStep $ combStep (combStep (map f as) bs) cs
