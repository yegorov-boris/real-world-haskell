module Chapter3
    ( len
    , mean
    , pal
    , isPal
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

--4. Turn a list into a palindrome; i.e., it should read the same both backward and forward. For example, given the list [1,2,3], your function should return [1,2,3,3,2,1].
pal :: [a] -> [a]
pal [] = []
pal x@[h] = x
pal x = x ++ reverse x

--5. Write a function that determines whether its input list is a palindrome.
isPal :: Eq a => [a] -> Bool
isPal x = let n = length x `div` 2 in take n x == take n (reverse x)
