module Chapter3
    ( len
    , mean
    , pal
    , isPal
    , sortByLen
    , myIntersperse
    , height
    , Tree (..)
    , direction
    , Direction (..)
    , directions
    , scanConvexHull
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
sortByLen = sortBy (\a b -> length a `compare` length b)

--7-8. Define a function that joins a list of lists together using a separator value
--The separator should appear between elements of the list, but it should not follow the last element
myIntersperse :: a -> [[a]] -> [a]
myIntersperse s xs = foldr f [] xs
  where
    f x [] = x
    f x acc = x ++ s:acc

--9. Using the binary tree type that we defined earlier in this chapter, write a function that will determine the height of the tree. The height is the largest number of hops from the root to an Empty
data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

height :: Tree a -> Int
height Empty = 0
height (Node _ Empty Empty) = 1
height (Node _ Empty r) = succ $! height r
height (Node _ l Empty) = succ $! height l
height (Node _ l r) = succ $! (max $! height l) $! height r

data Direction = TurnR | TurnL | Straight deriving (Show, Eq)

--10. Consider three two-dimensional points, a, b, and c. If we look at the angle formed by the line segment from a to b and the line segment from b to c, it turns left, turns right, or forms a straight line. Define a Direction data type that lets you represent these possibilities.
--11. Write a function that calculates the turn made by three two-dimensional points and returns a Direction.
direction :: (Ord a, Num a) => (a, a) -> (a, a) -> (a, a) -> Direction
direction (ax, ay) (bx, by) (cx, cy)
  | p > q = TurnR
  | p < q = TurnL
  | otherwise = Straight
  where
    p = (cx - bx) * (by - ay)
    q = (bx - ax) * (cy - by)

--12. Define a function that takes a list of two-dimensional points and computes the direction of each successive triple. Given a list of points [a,b,c,d,e], it should begin by computing the turn made by [a,b,c], then the turn made by [b,c,d], then [c,d,e]. Your function should return a list of Direction.
directions :: (Ord a, Num a) => [(a, a)] -> [Direction]
directions [] = []
directions [_] = []
directions [_, _] = []
directions [a, b, c] = [direction a b c]
directions (a:b:c:t) = (direction a b c):directions (b:c:t)

--13. Using the code from the preceding three exercises, implement Graham’s scan al- gorithm for the convex hull of a set of 2D points. You can find good description of what a convex hull (http://en.wikipedia.org/wiki/Convex_hull) is, and how the Graham scan algorithm (http://en.wikipedia.org/wiki/Graham_scan) should work, on Wikipedia (http://en.wikipedia.org/).
scanConvexHull :: [(Double, Double)] -> [(Double, Double)]
scanConvexHull [] = []
scanConvexHull [_] = []
scanConvexHull [_, _] = []
scanConvexHull points = graham [p1, p0] pts
  where
    graham s [] = s
    graham s (p:ps) = graham (p:checkStack p s) ps
    checkStack p s =
      if
        (head s == p1) || (direction (head $ tail s) (head s) p /= TurnR)
      then
        s
      else
        tail s
    p0 = minimumBy (\a b -> snd a `compare` snd b) points
    (p1:pts) = sortBy byAngleDist $ delete p0 points
    sub (ax, ay) (bx, by) = (bx - ax, by - ay)
    dist a b = let (x, y) = sub a b in x*x + y*y
    angle = uncurry (flip atan2) . sub p0
    byAngleDist a b
      | angle a > angle b = GT
      | angle a < angle b = LT
      | otherwise = dist p0 a `compare` dist p0 b
