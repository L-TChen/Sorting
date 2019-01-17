{-# LANGUAGE BangPatterns #-} 
{-# OPTIONS_GHC -O2 #-}

module Data.List.QuickX where 

import qualified Data.List as L
--import Control.Parallel.Strategies 

sort :: Ord a => [a] -> [a]
sort xs = sortapp xs [] 

sortapp []     rest = rest
sortapp (x:xs) rest = sortp x xs 0 0 [] [] [x] rest 

sortp x []     !n !m us vs ws rest 
  | m < n     = smsort us ++ ws ++ sortapp vs rest
  | otherwise = sortapp us (ws ++ smsort vs ++ rest)
sortp x (y:ys) !n !m us vs ws rest =
  case y `compare` x of
    LT -> sortp x ys (n+1) m     (y:us) vs     ws     rest
    GT -> sortp x ys n     (m+1) us     (y:vs) ws     rest
    EQ -> sortp x ys n     m us         vs     (y:ws) rest


sequences (a:b:xs)
  | a `compare` b == GT = descending b [a]  xs
  | otherwise       = ascending  b (a:) xs
sequences xs = [xs]

descending a as (b:bs)
  | a `compare` b == GT = descending b (a:as) bs
descending a as bs  = (a:as): sequences bs

ascending a as (b:bs)
  | a `compare` b /= GT = ascending b (\ys -> as (a:ys)) bs
ascending a as bs   = let !x = as [a]
                      in x : sequences bs

smsort :: Ord a => [a] -> [a]
smsort = mergeAll . sequences
  where
    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)

    mergePairs (a:b:xs) = let !x = merge a b
                          in x : mergePairs xs
    mergePairs xs       = xs

    merge as@(a:as') bs@(b:bs')
      | a `compare` b == GT = b:merge as  bs'
      | otherwise       = a:merge as' bs
    merge [] bs         = bs
    merge as []         = as
