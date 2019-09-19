module Chapter4
    ( safeHead
    , safeTail
    , safeLast
    , safeInit
    , splitWith
    , firstWords
    , transpose
    ) where

import Data.List (intercalate)

--1. Write your own “safe” definitions of the standard partial list functions, but make sure they never fail
safeHead :: [a] -> Maybe a
safeHead = safely head

safeTail :: [a] -> Maybe [a]
safeTail = safely tail

safeLast :: [a] -> Maybe a
safeLast = safely last

safeInit :: [a] -> Maybe [a]
safeInit = safely init

safely :: ([a] -> b) -> [a] -> Maybe b
safely _ [] = Nothing
safely f a = Just $ f a -- try point-free

--2. Write a function splitWith that acts similarly to words but takes a predicate and a list of any type, and then splits its input list on every element for which the predicate returns False
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs = case a of
  [] -> t
  _ -> a:t
  where
    a = takeWhile f xs
    t = splitWith f $ dropWhile (not . f) $ dropWhile f xs

--3. Using the command framework from the earlier section “A Simple Command-Line Framework” on page 71, write a program that prints the first word of each line of its input.
firstWords :: String -> String
firstWords = (intercalate "\n") . (map w) . splitLines
  where
    w "" = ""
    w l = head $ words l

splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
  let (pre, suf) = break isLineTerminator cs
  in pre : case suf of
    ('\r':'\n':rest) -> splitLines rest
    ('\r':rest) -> splitLines rest
    ('\n':rest) -> splitLines rest
    _ -> []

isLineTerminator :: Char -> Bool
isLineTerminator c = c == '\r' || c == '\n'

--4. Write a program that transposes the text in a file. For instance, it should convert "hello\nworld\n" to "hw\neo\nlr\nll\nod\n".
transpose :: String -> String
transpose s = intercalate "\n" $ transpose' l rows
  where
    rows = splitLines s
    l = maximum $ map length rows
    transpose' 0 _ = []
    transpose' n xs = map head' xs : transpose' (n - 1) (map tail' xs)
    head' [] = ' '
    head' a = head a
    tail' [] = []
    tail' a = tail a
