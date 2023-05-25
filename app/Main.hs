{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Tasty.Bench
import Control.Arrow
import Data.List (foldl')

import GHC.Utils.Misc


main :: IO ()
main = defaultMain
  [ bgroup (show sz)
    [ bgroup pr
      [ bgroup fn
        [ bench "predicate" $ nf (f $ p sz) [0..sz]
        , bench "in order"  $ whnf (\n -> inOrder n f p) sz
        , bench "flipped"   $ whnf (\n -> revOrder n f p) sz
        ] | (fn, f) <- fns] | (pr, p) <- predicates ] | sz <- listSizes] 


predicate1 :: Int -> Int -> Bool
predicate1 n x = x == (n `div` 2)


predicate2 :: Int -> Int -> Bool
predicate2 _ x = x == 0


predicate3 :: Int -> Int -> Bool
predicate3 n x = x == n


predicate4 :: Int -> Int -> Bool
predicate4 n x = x `elem` [0..n] && x == (n `div` 2)


listSizes :: [Int]
listSizes = [1,10,100,1000,10000,100000,1000000,10000000]


inOrder :: Int -> ((Int -> Bool) -> [Int] -> ([Int], [Int])) -> (Int -> Int -> Bool) -> Int
inOrder sz f p =
  let (xs, ys) = f (p sz) [0..sz]
  in foldl' (+) (foldl' (+) 0 xs) ys

revOrder :: Int -> ((Int -> Bool) -> [Int] -> ([Int], [Int])) -> (Int -> Int -> Bool) -> Int
revOrder sz f p =
  let (xs, ys) = f (p sz) [0..sz]
  in foldl' (+) (foldl' (+) 0 ys) xs


fns :: [(String, (Int -> Bool) -> [Int] -> ([Int], [Int]))]
fns =
  [
  --   ("breaklib", break)
  -- , ("spanlib" , span)
    ("break", break_)
  , ("break'", break')
  , ("breakStrict", breakStrict)
  -- , ("breaktake", breaktake)
  -- , ("breaktake2", breaktake2)
  -- , ("breaknotspan", breaknotspan)
  ]

consumers :: [(String, ([Int], [Int]) -> Int)]
consumers =
  [ ("in order", (\(xs, ys) -> foldl' (+) (foldl' (+) 0 xs) ys))
  , ("flipped",  (\(xs, ys) -> foldl' (+) (foldl' (+) 0 ys) xs))
  ]

predicates :: [(String, Int -> Int -> Bool)]
predicates =
  [ ("half", predicate1)
--  , ("zero", predicate2)
--  , ("full", predicate3)
--  , ("slow", predicate4)
  ]


span' :: (a -> Bool) -> [a] -> ([a], [a])
span' p = para' (\x xs -> if p x then first (x :) else const ([], x : xs)) ([], [])


span_ :: (a -> Bool) -> [a] -> ([a], [a])
span_ _ xs@[] = (xs, xs)
span_ p xs@(x:xs')
  | p x = let !(ys,zs) = span_ p xs' in (x:ys, zs)
  | otherwise = ([], xs)


para' :: forall a b. (a -> [a] -> b -> b) -> b -> [a] -> b
para' f z = go
  where
    go :: [a] -> b
    go [] = z
    go (x : xs) = let !goxs = go xs in f x xs goxs


break_ :: (a -> Bool) -> [a] -> ([a], [a])
break_ _ xs@[] = (xs, xs)
break_ p xs@(x:xs')
  | p x = ([], xs)
  | otherwise = let (ys, zs) = break_ p xs' in (x:ys, zs)


break' :: (a -> Bool) -> [a] -> ([a], [a])
break' _ xs@[] = (xs, xs)
break' p xxs@(x:xs)
  | p x = ([], xxs)
  | otherwise = let !(ys, zs) = break' p xs in (x:ys, zs)


breakStrict :: forall a. (a -> Bool) -> [a] -> ([a], [a])
breakStrict p = go []
  where
    go :: [a] -> [a] -> ([a], [a])
    go acc [] = (reverse acc, [])
    go acc xxs@(x : xs)
      | p x = (reverse acc, xxs)
      | otherwise = go (x : acc) xs


breakStrict' :: forall a. (a -> Bool) -> [a] -> ([a], [a])
breakStrict' p = go []
  where
    go :: [a] -> [a] -> ([a], [a])
    go acc [] = (reverse acc, [])
    go acc xxs@(x : xs)
      | p x = (reverse acc, xxs)
      | otherwise = go (x : acc) xs


breaktake :: (a -> Bool) -> [a] -> ([a], [a])
breaktake p xs = (hs, ts)
  where
    hs = takeWhile (not . p) xs
    ts = dropList hs xs


breaktake2 :: (a -> Bool) -> [a] -> ([a], [a])
breaktake2 p xs = (hs, ts)
  where
    hs = takeWhile (not . p) xs
    ts = dropWhile p xs


breaknotspan :: (a -> Bool) -> [a] -> ([a], [a])
breaknotspan p = span (not . p)
