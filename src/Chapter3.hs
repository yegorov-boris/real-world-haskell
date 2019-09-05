module Chapter3
    ( len
    , mean
    , pal
    , isPal
    , sortByLen
    , myIntersperse
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

--6. Create a function that sorts a list of lists based on the length of each sublist
sortByLen :: [[a]] -> [[a]]
sortByLen = sortBy cmpLen

cmpLen :: [a] -> [a] -> Ordering
cmpLen a b
  | length a < length b = LT
  | length a > length b = GT
  | otherwise = EQ

--7-8. Define a function that joins a list of lists together using a separator value
--The separator should appear between elements of the list, but it should not follow the last element
myIntersperse :: a -> [[a]] -> [a]
myIntersperse s xs = foldr f [] xs
  where
    f x [] = x
    f x acc = x ++ s:acc
