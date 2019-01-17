{-# LANGUAGE TypeApplications, ScopedTypeVariables, BangPatterns #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Data.List.Heap where

import Data.List.Merge 

data Splay a = SEmpty | SNode (Splay a) !a (Splay a)

empty = SEmpty
single x = SNode SEmpty x SEmpty

fromList :: (Ord a, Foldable t) => t a -> Splay a 
fromList = foldr insert empty 

toAscList t = tol t []
  where tol SEmpty rest = rest
        tol (SNode a x b) rest = tol a (x : tol b rest)

insert k t = SNode a k b
  where 
    (a, b) = partition t  -- elements of a <= k, elements of b > k

    partition SEmpty = (SEmpty,SEmpty)
    partition t@(SNode tl x tr)
      | x < k =
          case tr of
            SEmpty -> (t,SEmpty)
            SNode trl y trr
              | y < k ->
                  let tl' = SNode tl x trl
                      (lt,ge) = partition trr
                  in (SNode tl' y lt,ge)
              | otherwise ->
                  let (lt,ge) = partition trl
                  in (SNode tl x lt,SNode ge y trr)
      | otherwise =
          case tl of
            SEmpty -> (SEmpty,t)
            SNode tll y tlr
              | y < k ->
                  let (lt,ge) = partition tlr
                  in (SNode tll y lt,SNode ge x tr)
              | otherwise ->
                  let tr' = SNode tlr x tr
                      (lt,ge) = partition tll
                  in (lt,SNode ge y tr')

splaySort :: forall a. (Ord a) => [a] -> [a]
splaySort = toAscList . (fromList @a)

--data Leftist a = Empty 
--              | Node !Int a (Leftist a) (Leftist a)
--              deriving (Show, Read)
--
--pattern (:<) :: (Ord a) => a -> Leftist a -> Leftist a 
--pattern m :< h <- (removeMin -> (m, h)) 
--
--merge :: Ord a => Leftist a -> Leftist a -> Leftist a
--merge Empty h = h
--merge h Empty = h
--merge h1@(Node _ x1 a1 b1) h2@(Node _ x2 a2 b2) 
--  | x1 <= x2  = makeT x1 a1 (merge b1 h2)
--  | otherwise = makeT x2 a2 (merge h1 b2) 
--    where 
--      rank Empty = 0
--      rank (Node r _ _ _) = r 
--      makeT x a b 
--        | rank a >= rank b = Node (rank b + 1) x a b
--        | otherwise        = Node (rank a + 1) x b a
--
--insert :: Ord a => Leftist a -> a -> Leftist a
--insert h x = merge (Node 1 x Empty Empty) h
--
--findMin :: Ord a => Leftist a -> a
--findMin (Node _ x _ _) = x
--
--deleteMin :: Ord a => Leftist a -> Leftist a
--deleteMin (Node _ _ a b) = merge a b 
--
--removeMin :: Ord a => Leftist a -> (a, Leftist a)
--removeMin (Node _ m a b) = (m, merge a b)
--
--singleton :: a -> Leftist a 
--singleton x = Node 1 x Empty Empty
--
--fromList :: Ord a => [a] -> Leftist a 
--fromList = mergeAll . map singleton 
--  where
--    mergeAll [] = Empty
--    mergeAll [h] = h
--    mergeAll xs = mergeAll (mergePairs xs)
--
--    mergePairs (x:y:xs) = let h = merge x y in h:mergePairs xs 
--    mergePairs xs = xs

--ksmergesort :: (Ord a) => [a] -> [a] 
--ksmergesort = toAscList . fromList . sequences 
--  where 
--    toAscList Empty = []
--    toAscList ([x] :< h)   = x:toAscList h
--    toAscList ((x:xs):< h) = x:toAscList (merge (singleton xs) h)
--    toAscList ([] :< h)    = toAscList h
