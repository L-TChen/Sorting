{-# LANGUAGE BangPatterns, ParallelListComp #-}

import Quick
import Insertion 
import Merge 
import Data.List (sort)

import Control.Monad
import Test.QuickCheck 
import Criterion.Main


degrees = [2..4] 

main :: IO ()
main = do
  n <- generate $ choose (0 :: Int, 100)
  let sorted = [replicate (10 ^ i) n | i <- degrees]

  let ascending  = [ enumFromTo n (n + 10^i) | i <- degrees]
  let descending = reverse <$> ascending

  random <- forM degrees (\i ->
    generate $ vectorOf (10 ^ i) $ choose (0 :: Int, 10 ^ max 2 (i -2)))

  diverse <- forM degrees (\i -> 
    generate $ vectorOf (10 ^ i) $ choose (0 :: Int, 10 ^ i))

  defaultMain
    [ bgroup name 
      [ bgroup "Replicate" 
        [ bench ("10^" ++ show i) $ nf f xs | xs <- sorted | i <- degrees ]
      , bgroup "Random" 
        [ bench ("10^" ++ show i) $ nf f xs | xs <- random | i <- degrees ]
      , bgroup "Diverse" 
        [ bench ("10^" ++ show i) $ nf f xs | xs <- diverse | i <- degrees ]
      ]
    | (name, f) <- sortfuncs ] 

-- I couldn't get the following working 
--setupEnv = do
--  n <- generate $ choose (0 :: Int, 100)
--  let sorted = [replicate (10 ^ i) n    | i <- [3 .. 5]]
--               ++ [ [n .. n+10^i]       | i <- [3..  5]]
--               ++ [ reverse [n..n+10^i] | i <- [3 .. 5]]
--
--  random <- forM [3..5] (\i ->
--    generate $ vectorOf (10 ^ i) $ choose (0 :: Int, 10 ^ (max 2 $ i -2)))
--
--  diverse <- forM [3..5] (\i -> 
--    generate $ vectorOf (10 ^ i) $ choose (0 :: Int, 10 ^ i))
--
--  return (sorted, random, diverse) 
type Name = String 
type Sorting a = [a] -> [a]

check :: IO ()
check = mapM_ quickCheck [ prop f | (_, f) <- sortfuncs ]
  where prop f xs = f xs == sort xs 

sortfuncs :: [(Name, Sorting Int)]
sortfuncs = [ ("Data.List.sort",             sort)
            , ("GHC's Bottom-Up Mergesort",  smergesort)
            , ("Classic Bottom-Up Mergsort", mergesort)
            , ("Insertion Sort",             inssort)
            , ("Tail Call Insertion Sort",   inssort')
            , ("Newbie's Quicksort",         qsort)
            , ("Classic Haskell Quicksort",  hqsort)
            , ("Bird's Quicksort",           bqsort)
            , ("Bird's Quicksort+asending check", baqsort)
            , ("3-way Quicksort",            tqsort)
            , ("3-way Quicksort+ascending check", taqsort)
            , ("Tuned Quicksort",            xqsort)
            , ("Hybridsort", iqsort)
            ]
  
samples = 
  [ ("reverse [1.. 5000]",  reverse [1.. 5000])
  , ("reverse [1..10000]", reverse [1..10000])
  , ("[1..5000]", [1..5000])
  , ("[1..10000]", [1..10000])
  , ("replicate 5000 5", replicate 1000 5)
  , ("replicate 10000 5", replicate 1000 5)
  ]
