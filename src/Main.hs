{-# LANGUAGE BangPatterns, ParallelListComp, MonadComprehensions #-}

import Splay
import Quick
import Insertion 
import Merge 
import Data.List (sort, transpose)

import Control.Monad
import Test.QuickCheck 
import Criterion.Main

type Name = String 
type Sorting a = [a] -> [a]

check :: IO ()
check = mapM_ quickCheck [ prop f | (_, fs) <- sortfuncs, (_, f) <- fs ]
  where prop f xs = f xs == sort xs 

sortfuncs :: [(Name, [(Name, Sorting Int)])]
sortfuncs = 
  [ ("Heap Sort", heapsorts)
  , ("Insertion Sort", inssorts)
  , ("Quicksort", qssorts)
  , ("Mergesort", mergesorts)
  ]

heapsorts =
  [ ("Splay Sort", splaySort)
  ]

inssorts =
  [ ("Classic",    inssort)
  , ("Tail Tall",  inssort')
  ]

mergesorts =
  [ ("Data.List.sort", sort)
  , ("Smooth", smergesort)
  , ("Classic", mergesort)
  ]

qssorts = 
  [ ("Newbie",  qsort)
  , ("Classic",  hqsort)
  , ("Bird's",           bqsort)
  , ("3way",            tqsort)
  , ("3way+Accumulator", tqsorta)
  , ("3way+Ascending", taqsort)
  , ("Median of Medians", mqsort)
  , ("3way+Ascending+MedianOf3", xqsort)
  , ("Stable", q3s)
  , ("Insertion", iqsort)
  ]

random, diverse :: Int -> IO [Int] 
random i  = generate $ vectorOf (2^i) $ choose (0, 10^(i-1))
diverse i = generate $ vectorOf (2^i) $ choose (0, 10^(i+1))

sorted n i = replicate (2^i) n
ascending n i = concat $ replicate 3 $ enumFromTo n (n + 2^i)
descending n = reverse <$> ascending n
worst n i = concat $ transpose [enumFromTo n (n+2^i), enumFromTo (n-10) (n-10+2^i)]
pseudoRandom i = [(1299709 * 144737 - 10) `mod` x | x <- [100..2^i+100]] 

main :: IO ()
main = do
  n <- generate $ choose (0 :: Int, 100)

  let degrees = [16..20]
  let static f = 
        [ bench (desc ++ ":2^" ++ show i) $ nf f (xs i) 
        | (desc, xs) <- [ ("Replicate", sorted n)
                        , ("Ascending", ascending n)
                        , ("Descending", descending n)
                        , ("Adversary", worst n) 
                        , ("Pseudorandom", pseudoRandom)
                        ]
        , i <- degrees 
        ]

  randoms' <- mapM random degrees 
  let  randoms f = 
         [ bench ("Random:2^" ++ show i) $ nf f xs | xs <- randoms' | i <- degrees ]

  diverses' <- mapM diverse degrees
  let diverses f =
        [ bench ("Diverses:2^" ++ show i) $ nf f xs | xs <- diverses' | i <- degrees ]

  let testBenches = static <> randoms <> diverses

  defaultMain 
    [ bgroup name 
      [ bgroup subname (testBenches (length . f)) |  (subname, f) <- fs ]
    | (name, fs) <- sortfuncs ]

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
