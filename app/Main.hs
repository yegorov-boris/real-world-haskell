module Main where

import Chapter3

main :: IO ()
main = putStrLn $ show $ scanConvexHull [(5, 6), (3, 5), (5, 4), (4, 3), (5, 3), (3, 2)]
