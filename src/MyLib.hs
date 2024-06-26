module MyLib
  ( myLast,
    lastTwo,
    elementAt,
    myLength,
    myReverse,
    isPalindrome,
  )
where

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

{- Problem 3
 Find the nth element of a list. The first element in the list is number 1.
-}
elementAt :: Int -> [a] -> Maybe a
elementAt _ [] = Nothing
elementAt n (x : xs)
  | n == 1 = Just x
  | otherwise = elementAt (n - 1) xs

{- Problem 4
 Find the number of elements in a list.
-}
myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

-- myLength :: [a] -> Int
-- myLength = foldl (\acc _ -> acc + 1) 0

{- Problem 5
 Reverse items in a list
 Note: the cons operator always prepends an element to a list
-}
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []

{- Problem 6
 Determine if a list is a palindrome.
-}
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = (head xs) == (last xs) && (isPalindrome $ tail $ init xs)
