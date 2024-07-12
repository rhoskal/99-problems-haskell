module MyLib
  ( Encoded (..),
    NestedList (..),
    combinations,
    compress,
    decodeModified,
    dropEvery,
    duplicate,
    elementAt,
    encode,
    encodeDirect,
    encodeModified,
    flatten,
    goldbach,
    group,
    group3,
    insertAt,
    isPalindrome,
    isPrime,
    lastTwo,
    lfsort,
    lottoSelect,
    lsort,
    myLast,
    myLength,
    myReplicate,
    myReverse,
    pack,
    primeFactors,
    primeFactorsMult,
    primesFrom,
    range,
    removeAt,
    rndPermutations,
    rndSelect,
    rotate,
    slice,
    split,
  )
where

import Data.List (groupBy, sortBy, sortOn)
import System.Random (randomRIO)

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
split xs n
  | n <= 0 = ([], xs)
  | otherwise = (take n xs, drop n xs)

{- Problem 18
 Given two indices, i and k, the slice is the list containing the elements between
 the i'th and k'th element of the original list (both limits included).
 Start counting the elements with 1.
-}
slice :: [a] -> Int -> Int -> [a]
slice xs i k
  | k < i = []
  | otherwise = take (k - i + 1) $ drop (i - 1) xs

{- Problem 19
 Rotate a list n places to the left.
-}
rotate :: [a] -> Int -> [a]
rotate xs n
  | n < 0 = drop (n + length xs) xs ++ take (n + length xs) xs
  | otherwise = drop n xs ++ take n xs

{- Problem 20
 Removes the nth element from a list.
-}
removeAt :: Int -> [a] -> [a]
removeAt n xs
  | n < 0 = xs
  | otherwise = take (n - 1) xs ++ drop n xs

{- Problem 21
 Inserts an element at the nth position.
 Start counting list elements with 0. If the position is larger or equal to
 the length of the list, insert the element at the end.
 (The behavior is unspecified if the position is negative.)
-}
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n
  | n < 0 = xs
  | n >= length xs = xs ++ [x]
  | otherwise = take (n - 1) xs ++ x : drop (n - 1) xs

{- Problem 22
 Create a list containing all integers within a given range.
-}
range :: Int -> Int -> [Int]
range a b
  | b < a = []
  | otherwise = [a] ++ range (a + 1) b

-- range a b = take ((b - a) + 1) $ iterate (+ 1) a

{- Problem 23
 Extract a given number of randomly selected elements from a list.
 It's not clear if duplicates are allowed.
-}
rndSelect :: [a] -> Int -> IO [a]
rndSelect _ 0 = return []
rndSelect [] _ = return []
rndSelect xs n = do
  rnd <- randomRIO (0, (length xs) - 1)
  rest <- rndSelect xs (n - 1)
  return $ (xs !! rnd) : rest

{- Problem 24
 Extract n different random numbers from the set 1..M.
-}
lottoSelect :: Int -> Int -> IO [Int]
lottoSelect n m = rndSelect [1 .. m] n

{- Problem 25
 Generate a random permutation of the elements of a list.
 Note: this solution does not guarantee uniqueness (distinct elements) from `as`
-}
rndPermutations :: [a] -> IO [a]
rndPermutations [] = return []
rndPermutations xs = rndSelect xs (length xs)

{- Problem 26
 Generate combinations of k distinct objects chosen from the n elements of a list.
-}
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs =
  [ xs !! i : x
    | i <- [0 .. (length xs) - 1],
      x <- combinations (n - 1) $ drop (i + 1) xs
  ]

combinations' :: Int -> [a] -> [([a], [a])]
combinations' 0 xs = [([], xs)]
combinations' _ [] = []
combinations' n (x : xs) = selected ++ remaining
  where
    selected = [(x : ys, zs) | (ys, zs) <- combinations' (n - 1) xs]
    remaining = [(ys, x : zs) | (ys, zs) <- combinations' n xs]

{- Problem 27
 Group the elements of a set into 3 disjoint subsets.
-}
group3 :: [a] -> [[[a]]]
group3 = group [3]

{- Problem 28
 Generalized `group3` specifying a list of group sizes and the predicate will return a list of groups.
-}
group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (n : ns) xs =
  [ g : gs
    | (g, rs) <- combinations' n xs,
      gs <- group ns rs
  ]

{- Problem 29
 Sort the elements of a list according to their length.
 e.g. short lists first, longer lists later.
-}
lsort :: [String] -> [String]
lsort = sortOn length

{- Problem 30
 Sort the elements of a list according to their length frequency.
 e.g. in the default, where sorting is done ascendingly, lists with rare
 lengths are placed first, others with a more frequent length come later.
-}
lfsort :: [String] -> [String]
lfsort as = sortBy (\xs ys -> compare (frequency (length xs) as) (frequency (length ys) as)) as
  where
    frequency :: Int -> [String] -> Int
    frequency len = length . filter (\x -> length x == len)

-- Does not keep order of original list
-- lfsort :: [String] -> [String]
-- lfsort xs =
--   map snd $
--     concat $
--       sortOn length $
--         groupBy (\x y -> fst x == fst y) $
--           sortOn fst $
--             map (\x -> (length x, x)) xs

{- Problem 31
 Should return true if given number is prime.
 Note: uses "trivial division" which is the most basic algo.
 Optimizations: only check up to the sqrt n and skip evens after 2
-}
isPrime :: (Integral a) => a -> Bool
isPrime n
  | n < 2 = False
  | otherwise =
      all ((/= 0) . mod n) $
        takeWhile (\i -> i * i <= n) (2 : [i | i <- [3 ..], i `mod` 2 /= 0])

{- Problem 32
 Determine the prime factors of a given positive integer.
 Returns a flat list containing the prime factors in ascending order.
-}
primeFactors :: Int -> [Int]
primeFactors n = primeFactors' n 2
  where
    primeFactors' :: Int -> Int -> [Int]
    primeFactors' 1 _ = []
    primeFactors' num divisor
      | num `mod` divisor == 0 = divisor : primeFactors' (num `div` divisor) divisor
      | otherwise = primeFactors' num (divisor + 1)

{- Problem 33
 Determine the prime factors and their multiplicities of a given positive integer.
 Note: using `groupBy` instead of `group` from `Data.List` to avoid naming collision
-}
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = map (\x -> (head x, length x)) . groupBy ((==)) . primeFactors

{- Problem 34
 Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
-}
primesFrom :: Int -> Int -> [Int]
primesFrom lower upper = filter isPrime [lower .. upper]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

{- Problem 35
 Goldbach's conjecture - finds two prime numbers that sum up to a given even integer.
-}
goldbach :: Int -> Maybe (Int, Int)
goldbach n
  | n <= 2 = Nothing
  | mod n 2 /= 0 = Nothing
  | otherwise =
      safeHead
        [ (x, y)
          | x <- primesFrom 3 (n - 2),
            let y = n - x,
            isPrime y
        ]
