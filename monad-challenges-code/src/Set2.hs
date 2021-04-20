{-# language NoImplicitPrelude #-}
module Set2 where

import Prelude (minimum, maximum, head, tail, Monad (..), Maybe (..))
import MCPrelude
import Set4


--instance Show a => Show (Maybe a) where
--  show Nothing  = "Nothing"
--  show (Just x) = "Just " ++ show x
--
--instance Eq a => Eq (Maybe a) where
--  (==) Nothing Nothing   = True
--  (==) (Just x) (Just y) = x == y
--  (==) _ _               = False

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay xs = Just $ head xs

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay xs = Just $ tail xs


lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay x ((f,s):ts)
        | x == f    = Just s
        | otherwise = lookupMay x ts

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing

divMay n d = Just (n / d)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay xs = Just (maximum xs)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay xs = Just (minimum xs)


queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd s = case (lookupMay s gd) of
                    Nothing -> Nothing
                    Just xs -> case (tailMay xs) of
                                 Nothing -> Nothing
                                 Just tl -> case (maximumMay tl) of
                                              Nothing -> Nothing
                                              Just mx -> case (headMay xs) of
                                                           Nothing -> Nothing
                                                           Just hd -> divMay (fromIntegral mx) (fromIntegral hd)

queryGreek3 :: GreekData -> String -> Maybe Double
queryGreek3 gd s = do
  xs <- lookupMay s gd
  tl <- tailMay xs
  mx <- maximumMay tl
  hd <- headMay xs
  divMay (fromIntegral mx) (fromIntegral hd)

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain = (=<<)


--chain _ Nothing  = Nothing
--chain f (Just x) = f x

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = (>>=)
--link x f = chain f x

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gd s = let lm = lookupMay s gd
                       tm = lm >>= tailMay
                       mx = tm >>= maximumMay
                       hd = lm >>= headMay in
                    hd >>= (\y -> (mx >>= (\x -> divMay (fromIntegral x) (fromIntegral y))))
--                    link hd (\y -> (link mx $ \x -> divMay (fromIntegral x) (fromIntegral y)))

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
--addSalaries ss x y = let lx = lookupMay x ss
--                         ly = lookupMay y ss in
--                      lx >>= (\x -> ly >>= (\y -> Just (x + y))) 
addSalaries ss x y = do
  lx <- lookupMay x ss
  ly <- lookupMay y ss
  return $ lx + ly

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
--yLink f ma mb = (\f ma -> chain (\fab -> fab <$> ma) f) (f <$> ma) mb
--yLink f ma mb = ma `link` \x -> mb `link` \y -> Just (f x y)
yLink = liftM2

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 ss x y = liftM2 (+) (lookupMay x ss) (lookupMay y ss)

tailProd :: Num a => [a] -> Maybe a
tailProd = (=<<) (return . product) . tailMay

tailSum :: Num a => [a] -> Maybe a
tailSum = (=<<) (return . sum) . tailMay

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe = fmap
--transMaybe f ma = chain (return . f) ma


tailProd2 :: Num a => [a] -> Maybe a
tailProd2 = fmap product . tailMay

tailSum2 :: Num a => [a] -> Maybe a
tailSum2 = fmap sum . tailMay

tailMax :: Ord a => [a] -> Maybe a
tailMax = fmap maximum . tailMay

tailMin :: Ord a => [a] -> Maybe a
tailMin = fmap minimum . tailMay
