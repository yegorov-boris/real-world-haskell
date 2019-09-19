module Chapter4
    ( safeHead
    , safeTail
    , safeLast
    , safeInit
    , splitWith
    , firstWords
    , transpose
    , asInt_either
    ) where

import Data.List (intercalate, foldl')

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

--1-4
asInt_either :: String -> Either String Int
asInt_either "" = Right 0
asInt_either ('-':t) = negate <$> asInt_either t
asInt_either s =
  let
    calculate (base, f) = f (base, 0)
  in
    (snd . calculate) <$> foldl' processDigit (Right (10, id)) s
  where
    step :: Int -> (Int, Int) -> (Int, Int)
    step digit (base, number) = (base, base * number + digit)
    processDigit ::   Either String (Int, (Int, Int) -> (Int, Int))
                   -> Char
                   -> Either String (Int, (Int, Int) -> (Int, Int))
    processDigit result d = case result of
      (Left e) -> Left e
      (Right _) -> case toDigit d of
        (Left e) -> Left e
        (Right i) -> (\(base, f) -> (getBase i base, step i . f)) <$> result
    getBase :: Int -> Int -> Int
    getBase n b = if n > 9 then 16 else b

toDigit :: Char -> Either String Int
toDigit '0' = Right 0
toDigit '1' = Right 1
toDigit '2' = Right 2
toDigit '3' = Right 3
toDigit '4' = Right 4
toDigit '5' = Right 5
toDigit '6' = Right 6
toDigit '7' = Right 7
toDigit '8' = Right 8
toDigit '9' = Right 9
toDigit 'a' = Right 10
toDigit 'A' = Right 10
toDigit 'b' = Right 11
toDigit 'B' = Right 11
toDigit 'c' = Right 12
toDigit 'C' = Right 12
toDigit 'd' = Right 13
toDigit 'D' = Right 13
toDigit 'e' = Right 14
toDigit 'E' = Right 14
toDigit 'f' = Right 15
toDigit 'F' = Right 15
toDigit c = Left $ "non-digit '" ++ c:"'"
