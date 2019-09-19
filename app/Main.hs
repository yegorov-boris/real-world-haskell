module Main where

import System.Environment (getArgs)
import Chapter4 (transpose)

main :: IO ()
main = mainWith myFunction
  where
    mainWith function = do
      args <- getArgs
      case args of
        [input,output] -> interactWith function input output
        _ -> putStrLn "error: exactly two arguments needed"
    myFunction = transpose

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)
