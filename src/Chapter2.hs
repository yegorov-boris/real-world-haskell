module Chapter2
    ( lastButOne
    ) where

-- 2. Write a function, lastButOne, that returns the element before the last.
lastButOne :: [a] -> a
lastButOne [] = error "too short"
lastButOne [x] = error "too short"
lastButOne [a, b] = a
lastButOne (h:t) = t `seq` lastButOne t
