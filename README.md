# Sorting
A comparsion of various sorting algorithms in Haskell

The built-in `Data.List.sort` in GHC 8.4.4 is embarrasingly slow compared to 
built-in sorting functions in other language, e.g., C, C++, etc.
The common wisdom (e.g., [here](https://stackoverflow.com/questions/52237695/why-does-haskell-use-mergesort-instead-of-quicksort/54067493?noredirect=1#comment95026154_54067493)
and [here](https://en.wikibooks.org/wiki/Haskell/Higher-order_functions)) suggest that sorting in Haskell is unavoidably slow
due to persistence, the sin of list, the requirement of maintaining stability (in [Haskell 98](https://www.haskell.org/onlinereport/list.html) report), and lots of plausible reasons. 

However, an experiment cooked in a few days shows that a threeway Quicksort properly written
is about twice faster than the smooth Mergesort in GHC in the case of 2^20 pseudorandom numbers. Here is the simplest one:

```
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
```
with the result on my machine
```
benchmarking Quicksort/3way+Accumulator/Pseudorandom:2^20
time                 1.161 s    (1.102 s .. 1.199 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.150 s    (1.137 s .. 1.158 s)
std dev              13.89 ms   (215.1 μs .. 19.63 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Mergesort/Data.List.sort/Pseudorandom:2^20
time                 2.335 s    (2.232 s .. 2.420 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.347 s    (2.325 s .. 2.366 s)
std dev              23.99 ms   (19.41 ms .. 28.07 ms)
variance introduced by outliers: 19% (moderately inflated)
```

The result itself is not very interesting, but investigating how to improve it and learning new general functional techniques 
to improve effiency are interesting. So, I created this repository to record different sorting algorithm implementations.

## Benchmark script

To run the benchmark by yourself, execute

```
cabal build
```

to compile the benchmark program. You will get a benchmark program using [criterion](http://www.serpentine.com/criterion/tutorial.html). 
Without any extra options, the program would simply run all possible cases and all implementation.
Use `--list` to check what you want to test.

##  Epilogue
An american computer scientist Alan J. Perlis once commented 

> A LISP programmer knows the value of everything, but the cost of nothing.

Functional programming is elegant and highly generic, but can it be efficient too? 
