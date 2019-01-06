{-# LANGUAGE BangPatterns #-}

module Quick where

import Insertion (inssort)

iqsort, qsort, xqsort, hqsort, bqsort, baqsort, tqsort, taqsort :: (Ord a) => [a] -> [a] 

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

baqsort [] = []
baqsort (x:xs) = sortp xs [] [] 
  where
    sortp []     us vs = baqsort us ++ (x:baqsort vs)
    sortp (y:ys) us vs = 
      if y < x then ascending ys y (y:us) vs else sortp ys us (y:vs)

    ascending [] _ us vs = us ++ (x:baqsort vs) 
    ascending (y:ys) y' us vs =
      if y < x
      then if y <= y' 
           then ascending    ys y (y:us) vs
           else notascending ys (y:us) vs
      else ascending ys y' us (y:vs)

    notascending []     us vs = baqsort us ++ (x:baqsort vs)
    notascending (y:ys) us vs = 
      if y < x then notascending ys (y:us) vs else notascending ys us (y:vs)

tqsort [] = []
tqsort (x:xs) = sortp xs [] [x] [] 
  where
    sortp [] us ws vs     = tqsort us ++ ws ++ tqsort vs
    sortp (y:ys) us ws vs =
      case compare y x of 
        LT -> sortp ys (y:us) ws vs 
        GT -> sortp ys us ws (y:vs)
        _  -> sortp ys us (y:ws) vs

taqsort [] = []
taqsort (x:xs) = sortp xs [] [x] [] 
  where
    sortp [] us ws vs     = taqsort us ++ ws ++ taqsort vs
    sortp (y:ys) us ws vs =
      case compare y x of
        GT -> sortp ys us ws (y:vs)
        LT -> ascending ys y (y:us) ws vs
        _  -> sortp ys us (x:ws) vs

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
iqsort (x:xs) = sortp xs [] [] 1
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

xqsort xs@(x:_) = xqsortWithPivot x xs
xqsort [] = []

xqsortWithPivot :: Ord a => a -> [a] -> [a]
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
    notascending [] !b !y us@(u:_) ws vs = 
      let mid = if u > b then b else max u y -- median of the last, the head, and the first peak 
      in xqsortWithPivot mid us ++ ws ++ xqsort vs
