{-# language NoImplicitPrelude #-}
module Set1 where

import MCPrelude
import Set4

--fiveRands :: [Integer]
--fiveRands = take 5 (go $ mkSeed 1)
--  where go :: Seed -> [Integer]
--        go s = let r = rand s
--                in (fst r : go (snd r))

randLetter :: Gen Char
randLetter = generalA toLetter (Gen rand)

--randString3 :: String
--randString3 = liftM3 (:) randLetter randLetter randLetter
--randString3 = take 3 (go $ mkSeed 1)
--  where go :: Seed -> String
--        go s = let r = randLetter s
--                in [fst r] ++ go (snd r)

randEven :: Gen Integer
randEven = generalA (*2) (Gen rand)

randOdd :: Gen Integer
randOdd = generalA (+1) randEven

randTen :: Gen Integer
randTen = generalA (*10) (Gen rand)

generalA :: (a -> b) -> Gen a -> Gen b
generalA f = (=<<) (return . f)
--generalA f ga = genTwo ga (return . f)

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair = liftM2 (,)
--generalPair ga gb s = let (x, sd) = getGen ga s
--                       in Gen ((x, fst $ getGen gb sd), sd)
--generalPair2 :: Gen a -> Gen b -> Gen (a, b)
--generalPair2 ga gb = generalB (,) ga gb

randPair :: Gen (Char, Integer)
randPair = generalPair randLetter (Gen rand)
--randPair s = let (c, sd) = randLetter s
--              in ((c, fst $ rand sd), sd)

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB = liftM2
--generalB f ga gb s = let (x, sd) = ga s
--                      in (f x (fst $ gb sd), sd)
--generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
--generalB2 f ga gb = ga `genTwo` \x -> gb `genTwo` \y -> return (f x y) 


repRandom :: [Gen a] -> Gen [a]
repRandom = sequence
--repRandom []       = (,) []
--repRandom (ga:gas) = generalB (:) ga (repRandom gas)
--repRandom2 gas = foldr (\ga gas -> (generalA (:) ga `genTwo` \f -> generalA f gas)) (return []) gas 

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo = (>>=)
