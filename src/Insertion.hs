module Insertion where 

import Data.List

inssort, inssort' :: (Ord a) => [a] -> [a] 

inssort = foldr insert [] 
inssort' = foldl' (flip insert) []
