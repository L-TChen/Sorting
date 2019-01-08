> module Splay where
> data Splay a = SEmpty | SNode (Splay a) a (Splay a)
>
> empty = SEmpty
> single x = SNode SEmpty x SEmpty
>
> fromList xs = foldr insert empty xs
> 
> toOrderedList t = tol t []
>   where tol SEmpty rest = rest
>         tol (SNode a x b) rest = tol a (x : tol b rest)
>
> insert k t = SNode a k b
>   where 
>     (a, b) = partition t  -- elements of a <= k, elements of b > k
>
>     partition SEmpty = (SEmpty,SEmpty)
>     partition t@(SNode tl x tr)
>       | x < k =
>           case tr of
>             SEmpty -> (t,SEmpty)
>             SNode trl y trr
>               | y < k ->
>                   let tl' = SNode tl x trl
>                       (lt,ge) = partition trr
>                   in (SNode tl' y lt,ge)
>               | otherwise ->
>                   let (lt,ge) = partition trl
>                   in (SNode tl x lt,SNode ge y trr)
>       | otherwise =
>           case tl of
>             SEmpty -> (SEmpty,t)
>             SNode tll y tlr
>               | y < k ->
>                   let (lt,ge) = partition tlr
>                   in (SNode tll y lt,SNode ge x tr)
>               | otherwise ->
>                   let tr' = SNode tlr x tr
>                       (lt,ge) = partition tll
>                   in (lt,SNode ge y tr')
>
> splaySort			:: (Ord a) => [a] -> [a]
> splaySort			=  toOrderedList
>				.  (fromList :: (Ord a) => [a] -> Splay a)
