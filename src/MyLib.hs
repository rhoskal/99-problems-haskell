module MyLib
  ( Encoded (..),
    NestedList (..),
    compress,
    decodeModified,
    dropEvery,
    duplicate,
    elementAt,
    encode,
    encodeDirect,
    encodeModified,
    flatten,
    isPalindrome,
    lastTwo,
    myLast,
    myLength,
    myReplicate,
    myReverse,
    pack,
    split,
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

{- Problem 7
 Flatten a nested list structure.
 Note: We have to define a new data type, because lists in Haskell are homogeneous.
 [1, [2, [3, 4], 5]] is a type error. Therefore, we must have a way of representing a list that may (or may not) be nested.
-}
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x : xs)) = flatten x ++ flatten (List xs)

-- flatten (Elem x) = [x]
-- flatten (List x) = concatMap flatten x

{- Problem 8
 Eliminate consecutive duplicates of list elements.
-}
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x : xs) = [x] ++ (compress $ dropWhile (== x) xs)

{- Problem 9
 Pack consecutive duplicates of list elements into sublists.
-}
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x : xs) =
  let (y, ys) = span (== x) xs
   in (x : y) : pack ys

{- Problem 10
 Runs the "run-length" encoding data compression algorithm.
 Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E..
-}
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack

data Encoded a
  = MultipleEncode a Int
  | SingleEncode a
  deriving (Eq, Show)

{- Problem 11
 Runs the "run-length" encoding data compression algorithm.
 Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E..
-}
encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified =
  map
    ( \(count, val) ->
        if count == 1
          then SingleEncode val
          else MultipleEncode val count
    )
    . encode

{- Problem 12
 Runs the "run-length" encoding data compression algorithm.
-}
decodeModified :: [Encoded a] -> [a]
decodeModified [] = []
decodeModified ((SingleEncode val) : xs) = val : decodeModified xs
decodeModified ((MultipleEncode val count) : xs) = (replicate count val) ++ decodeModified xs

{- Problem 13
 Run-length encoding of a list (direct solution).
 Implement the so-called run-length encoding data compression method directly.
-}
encodeDirect :: (Eq a) => [a] -> [Encoded a]
encodeDirect [] = []
encodeDirect xs =
  let hd = head xs
      (matches, rest) = span (== hd) xs
      encoded =
        if length matches == 1
          then SingleEncode hd
          else MultipleEncode hd (length matches)
   in encoded : encodeDirect rest

{- Problem 14
 Duplicate each item in a given list.
-}
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x : xs) = [x, x] ++ duplicate xs

{- Problem 15
 Replicate each item in a given list n number of times.
-}
myReplicate :: [a] -> Int -> [a]
myReplicate [] _ = []
myReplicate xs n = foldl (\acc x -> acc ++ repli n x) [] xs
  where
    repli :: Int -> a -> [a]
    repli n' = take n' . repeat

{- Problem 16
 Drop every nth item in a given list.
-}
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = take (n - 1) xs ++ dropEvery (drop n xs) n

{- Problem 17
 Splits a list into two parts; the length of the first part is given.
-}
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)
