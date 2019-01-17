module Data.List.Merge (mergesort, smergesort, sequences) where
      
mergesort :: Ord a => [a] -> [a]
mergesort = mergeAll . map (:[]) 
  where
    mergeAll []  = []
    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)

    mergePairs (a:b:xs) = let !x = merge a b
                          in x : mergePairs xs
    mergePairs xs       = xs

    merge as@(a:as') bs@(b:bs')
      | a > b     = b:merge as  bs'
      | otherwise = a:merge as' bs
    merge [] bs   = bs
    merge as []   = as

smergesort :: Ord a => [a] -> [a]
smergesort = sortBy compare

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = mergeAll . sequencesBy cmp 
  where
    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)

    mergePairs (a:b:xs) = let !x = merge a b
                          in x : mergePairs xs
    mergePairs xs       = xs

    merge as@(a:as') bs@(b:bs')
      | a `cmp` b == GT = b:merge as  bs'
      | otherwise       = a:merge as' bs
    merge [] bs         = bs
    merge as []         = as

sequences :: (Ord a) => [a] -> [[a]] 
sequences = sequencesBy compare

sequencesBy :: (a -> a -> Ordering) -> [a] -> [[a]]
sequencesBy cmp (a:b:xs)
  | a `cmp` b == GT = descending b [a]  xs
  | otherwise       = ascending b (a:) xs
    where
      descending a as (b:bs)
        | a `cmp` b == GT = descending b (a:as) bs
      descending a as bs  = (a:as): sequencesBy cmp bs
      ascending a as (b:bs)
        | a `cmp` b /= GT = ascending b (\ys -> as (a:ys)) bs
      ascending a as bs   = let !x = as [a]
                            in x : sequencesBy cmp bs
sequencesBy cmp xs = [xs]
