module MyLib (myLast, lastTwo) where

{- Problem 1
 Write a function that returns the last element of a list.
-}
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (_ : xs) = myLast $ tail xs

{- Problem 2
 Find the last two (last and penultimate) elements of a list.
-}
lastTwo :: [a] -> Maybe [a]
lastTwo [] = Nothing
lastTwo [_] = Nothing
lastTwo [x, y] = Just [x, y]
lastTwo (_ : xs) = lastTwo xs
