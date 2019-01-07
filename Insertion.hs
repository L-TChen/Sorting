module Insertion (inssort, insert) where 

import Data.List (insert, foldl') 

inssort, inssort' :: (Ord a) => [a] -> [a] 

inssort = foldr insert [] 
inssort' = foldl' (flip insert) []
