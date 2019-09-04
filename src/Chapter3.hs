module Chapter3
    ( len
    , mean
    ) where

import Data.List

--1. Write a function that computes the number of elements in a list. To test it, ensure that it gives the same answers as the standard length function.
--2. Addatypesignatureforyourfunctiontoyoursourcefile.Totestit,loadthesource file into ghci again.
len :: [a] -> Int
len x = foldl' (const . succ) 0 x

--3. Write a function that computes the mean of a list, i.e., the sum of all elements in the list divided by its length.
mean :: Fractional a => [a] -> a
mean [] = 0
mean x = let l = len x in foldl' (\s h -> s + h / fromIntegral l) 0 x
