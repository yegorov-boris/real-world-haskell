module Chapter3
    ( len
    ) where

--1. Write a function that computes the number of elements in a list. To test it, ensure that it gives the same answers as the standard length function.
--2. Addatypesignatureforyourfunctiontoyoursourcefile.Totestit,loadthesource file into ghci again.
len :: [a] -> Int
len x = len' x 0

len' :: [a] -> Int -> Int
len' [] l = l
len' (h:t) l = len' t $! succ l
