module MyLib (myLast) where

{- Problem 1
 Write a function that returns the last element of a list.
-}
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (_ : xs) = myLast $ tail xs
