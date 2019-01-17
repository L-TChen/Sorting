{-# LANGUAGE BangPatterns #-}

module Data.List.Quick where

import Data.List (insert) 
import Data.List.Insertion (inssort)

iqsort, qsort, hqsort, bqsort, tqsort, taqsort, q3s :: (Ord a) => [a] -> [a] 

qsort [] = [] 
qsort (x:xs) = qsort ys ++ x:qsort zs 
  where ys = filter (<x)  xs
        zs = filter (>=x) xs

hqsort [] = []
hqsort (x:xs) = hqsort ys ++ x:hqsort zs
  where (ys, zs) = partition (<x) xs 

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p = foldr op ([], []) 
  where
    op x (ys, zs) 
      | p x       = (x:ys, zs)
      | otherwise = (ys, x:zs)

bqsort [] = []
bqsort (x:xs) = sortp xs [] [] 
  where
    sortp []     us vs = bqsort us ++ (x:bqsort vs)
    sortp (y:ys) us vs = 
      if y < x then sortp ys (y:us) vs else sortp ys us (y:vs)

tqsort [] = []
tqsort (x:xs) = sortp xs [] [x] [] 
  where
    sortp [] us ws vs     = tqsort us ++ ws ++ tqsort vs
    sortp (y:ys) us ws vs =
      case compare y x of 
        LT -> sortp ys (y:us) ws vs 
        GT -> sortp ys us ws (y:vs)
        _  -> sortp ys us (y:ws) vs

tqsorta xs = tqsort' xs [] 

tqsort' :: (Ord a) => [a] -> [a] -> [a] 
tqsort' [] rest = rest
tqsort' (x:xs) rest = sortp xs [] [x] [] rest
  where
    sortp [] us ws vs rest    = tqsort' us (ws ++ tqsort' vs rest)
    sortp (y:ys) us ws vs rest  =
      case compare y x of 
        LT -> sortp ys (y:us) ws vs rest
        GT -> sortp ys us ws (y:vs) rest
        _  -> sortp ys us (y:ws) vs rest

taqsort [] = []
taqsort (x:xs) = sortp xs [x] [] 
  where
    sortp [] ws vs     = ws ++ taqsort vs
    sortp (y:ys) ws vs =
      case compare y x of
        GT -> sortp ys ws (y:vs)
        LT -> ascending ys y [y] ws vs
        _  -> sortp ys (x:ws) vs

    ascending [] _ us ws vs = us ++ ws ++ taqsort vs
    ascending (y:ys) y' us ws vs =
      case compare y x of 
        GT -> ascending ys y' us ws (y:vs)
        LT -> if y <= y' then ascending ys y (y:us) ws vs else notascending ys (y:us) ws vs
        _  -> ascending ys y' us (y:ws) vs

    notascending [] us ws vs = taqsort us ++ ws ++ taqsort vs
    notascending (y:ys) us ws vs =
      case compare y x of
        GT -> notascending ys us ws (y:vs)
        LT -> notascending ys (y:us) ws vs 
        _  -> notascending ys us (y:ws) vs


iqsort []     = []
iqsort (x:xs) = sortp xs [] [] (1 :: Int)
  where
    sortp []     us vs _  = inssort us ++ x:inssort vs
    sortp (y:ys) us vs !n
      | n < 16 = if y < x
        then sortp ys (y:us) vs (1+n)
        else sortp ys us (y:vs) (1+n)
      | otherwise = sortp' (y:ys) us vs 

    sortp' []     us vs = iqsort us ++ (x:iqsort vs) 
    sortp' (y:ys) us vs =
      if y < x then sortp' ys (y:us) vs else sortp' ys us (y:vs)

--
medianOfMedians :: (Ord a) => [a] -> a
medianOfMedians [x] = x
medianOfMedians xs = medianOfMedians (medianOfFives xs)

medianOfFives (x0:x1:x2:x3:x4:xs) = 
  let !m = median x0 x1 x2 x3 x4
  in m:medianOfFives xs 
medianOfFives [] = [] 
medianOfFives xs = [ ys !! ((n - 1) `div` 2)] 
  where
    (n, ys) = inssort' xs 
    inssort' = foldr op (0, [])
      where op x (!n, xs) = (n+1, insert x xs) 

{-# INLINE median #-}
median :: (Ord a) => a -> a -> a -> a -> a -> a
median x0 x1 x2 x3 x4 = insert x0 (insert x1 $ insert x2 $ insert x3 [x4]) !! 2

--
mqsort :: Ord a => [a] -> [a]
mqsort xs@(x:_) = mqsortWithPivot x xs
mqsort [] = []

mqsortWithPivot :: Ord a => a -> [a] -> [a]
mqsortWithPivot p xs = sortp xs (0 :: Int) [] [] [] (0 :: Int) [] [] 
  where 
    sortp (y:ys) !n us ums ws !l vs vms = 
      case compare y p of
        GT -> if l < 5
              then sortp ys n us ums ws (l+1) (y:vs) (y:vms)
              else sortp ys n us ums ws 0     (y:vs) (medianOfFive (y:vms))
        LT -> if n < 5
              then sortp ys (n+1) (y:us) (y:ums)                ws l vs vms
              else sortp ys 0     (y:us) (medianOfFive (y:ums)) ws l vs vms 
        EQ -> sortp ys n us ums (y:ws) l vs vms
    sortp []     _  []       _   ws _  []         _ = ws
    sortp []     _  []       _   ws _  vs@(_:_) vms = ws ++ mqsortWithPivot (medianOfMedians vms) vs 
    sortp []     _  us@(_:_) ums ws _  []         _ = mqsortWithPivot (medianOfMedians ums) us ++ ws
    sortp []     _  us       ums ws _  vs       vms = 
      mqsortWithPivot (medianOfMedians ums) us ++ ws ++ mqsortWithPivot (medianOfMedians vms) vs 

medianOfFive :: Ord a => [a] -> [a] 
medianOfFive (x0:x1:x2:x3:x4:xs) = 
  let !m = median x0 x1 x2 x3 x4
  in m:xs 
--medianOfFives xs = [ ys !! ((n - 1) `div` 2)] 
--  where (n, ys) = inssort' xs 

xqsort :: (Integral a, Ord a) => [a] -> [a]
xqsort xs@(x:_) = xqsortWithPivot x xs
xqsort [] = []

xqsortWithPivot :: (Ord a, Integral a) => a -> [a] -> [a]
xqsortWithPivot p xs = sortp xs [] [] 
  where
    sortp (y:ys) ws vs =
      case compare y p of
        GT -> sortp ys ws (y:vs)
        EQ -> sortp ys (y:ws) vs
        _  -> ascending ys y y [y] ws vs
    sortp []     ws vs = ws ++ xqsort vs

    ascending (z:zs) !z' y us ws vs =
      case compare z p of 
        GT -> ascending zs z' y us ws (z:vs)
        LT -> if z <= z' 
              then ascending    zs z y (z:us) ws vs
              else notascending zs z y (z:us) ws vs 
        _  -> ascending zs z' y us (z:ws) vs
    ascending []     _   _ us ws vs = us ++ ws ++ xqsort vs

    notascending (a:as) b y us ws vs =
      case compare a p of
        GT -> notascending as b y us ws (a:vs)
        LT -> notascending as b y (a:us) ws vs 
        _  -> notascending as b y us (a:ws) vs
    notascending [] !b !y us ws vs = 
      xqsortWithPivot ((b + y + head us) `div` 3) us ++ ws ++ xqsort vs

-- By Will Ness from Stack Overflow:
-- https://stackoverflow.com/questions/11355621/pseudo-quicksort-time-complexity/11373542#11373542
q3s xs = go xs [] where
  go     (x:xs) z = part x xs  go  (x:)  (`go` z)
  go     []     z = z
  part x (y:ys)  a  b  c =
      case compare y x of
                  LT -> part x ys  (a . (y:))  b  c
                  EQ -> part x ys  a  (b . (y:))  c
                  GT -> part x ys  a  b  (c . (y:))
  part _ []      a  b  c = a [] (b (c []))
