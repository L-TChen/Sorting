{-# LANGUAGE BangPatterns, ParallelListComp #-}

import Quick
import Insertion 
import Merge 
import Data.List (sort, transpose)

import Control.Monad
import Test.QuickCheck 
import Criterion.Main


degrees = [2..6] 

main :: IO ()
main = do
  n <- generate $ choose (0 :: Int, 100)
  let sorted = [replicate (10 ^ i) n | i <- degrees]

  let ascending  = concat $ replicate 3 [ enumFromTo n (n + 10^i) | i <- degrees]
  let descending = reverse <$> ascending
  let worst = [ concat $ transpose [enumFromTo n (n+10^i), enumFromTo (n-10) (n-10+10^i)] | i <- degrees]

  random <- forM degrees (\i ->
    generate $ vectorOf (10 ^ i) $ choose (0 :: Int, 10 ^ max 2 (i-1)))

  diverse <- forM degrees (\i -> 
    generate $ vectorOf (10 ^ i) $ choose (0 :: Int, 10 ^ i))

  defaultMain
    [ bgroup name 
      [ bgroup "Replicate" 
        [ bench ("10^" ++ show i) $ nf f xs | xs <- sorted | i <- degrees ]
      , bgroup "Ascending" 
        [ bench ("10^" ++ show i) $ nf f xs | xs <- ascending | i <- degrees ]
      , bgroup "Descending" 
        [ bench ("10^" ++ show i) $ nf f xs | xs <- descending | i <- degrees ]
      , bgroup "Worst Case" 
        [ bench ("10^" ++ show i) $ nf f xs | xs <- worst | i <- degrees ]
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
--            , ("Tail Call Insertion Sort",   inssort')
            , ("Newbie's Quicksort",         qsort)
            , ("Classic Haskell Quicksort",  hqsort)
            , ("Bird's Quicksort",           bqsort)
            , ("3-way Quicksort",            tqsort)
            , ("3-way Quicksort+ascending check", taqsort)
            , ("Quicksort+Median of Medians", mqsort)
            , ("Tuned 3-way Quicksort", xqsort)
            , ("Stable 3-way Quicksort", q3s)
            , ("Hybridsort", iqsort)
            ]
