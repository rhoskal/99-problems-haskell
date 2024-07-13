module MyLibSpec (spec) where

import BinaryTree (BinaryTree (..))
import Boolean
import MyLib
  ( Encoded (..),
    NestedList (..),
    TTRow (..),
    TruthTable (..),
    combinations,
    compress,
    coprime,
    decodeModified,
    dropEvery,
    duplicate,
    elementAt,
    encode,
    encodeDirect,
    encodeModified,
    flatten,
    gray,
    group,
    group3,
    huffman,
    insertAt,
    isBinaryTree,
    isPalindrome,
    isPrime,
    isSymmetricTree,
    lastTwo,
    lfsort,
    lottoSelect,
    lsort,
    myGCD,
    myLast,
    myLength,
    myReplicate,
    myReverse,
    pack,
    phi,
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
    table,
    totientPhi,
    unsafeGoldbach,
    unsafeGoldbachList,
  )
import Test.Hspec

spec :: Spec
spec = do
  it "[01] Should return the last element of a list" $ do
    myLast [] `shouldBe` (Nothing :: Maybe String)
    myLast ["a"] `shouldBe` (Just "a" :: Maybe String)
    myLast [1, 2, 3] `shouldBe` (Just 3 :: Maybe Int)

  it "[02] Should return the last two elements of a list" $ do
    lastTwo [] `shouldBe` (Nothing :: Maybe [Bool])
    lastTwo [True] `shouldBe` (Nothing :: Maybe [Bool])
    lastTwo [1, 2, 3] `shouldBe` (Just [2, 3] :: Maybe [Int])

  it "[03] Should return the nth element of a list" $ do
    elementAt (-2) [1, 2] `shouldBe` (Nothing :: Maybe Int)
    elementAt 0 [1, 2] `shouldBe` (Nothing :: Maybe Int)
    elementAt 2 [] `shouldBe` (Nothing :: Maybe Int)
    elementAt 2 [1, 2] `shouldBe` (Just 2 :: Maybe Int)
    elementAt 2 ['a' .. 'e'] `shouldBe` (Just 'b' :: Maybe Char)

  it "[04] Should return the number of elements in a list" $ do
    myLength [] `shouldBe` 0
    myLength [(1 :: Int)] `shouldBe` 1
    myLength [(1 :: Int) .. 10] `shouldBe` 10

  it "[05] Should return the elements in a list reversed" $ do
    myReverse [] `shouldBe` ([] :: [Int])
    myReverse [1 .. 5] `shouldBe` ([5, 4, 3, 2, 1] :: [Int])
    myReverse "A man, a plan, a canal, panama!" `shouldBe` ("!amanap ,lanac a ,nalp a ,nam A" :: [Char])

  it "[06] Should return true if a list is a palindrome" $ do
    isPalindrome ([] :: [Int]) `shouldBe` True
    isPalindrome ([1] :: [Int]) `shouldBe` True
    isPalindrome ([1, 2, 3] :: [Int]) `shouldBe` False
    isPalindrome (["a", "b", "a"] :: [String]) `shouldBe` True
    isPalindrome ([1, 2, 2, 1] :: [Int]) `shouldBe` True
    isPalindrome (["x", "a", "m", "a", "x"] :: [String]) `shouldBe` True
    isPalindrome ("madamimadam" :: [Char]) `shouldBe` True
    isPalindrome ([1, 2, 4, 8, 16, 8, 4, 2, 1] :: [Int]) `shouldBe` True

  it "[07] Should return a flattened list" $ do
    flatten (List [Elem 1, List [Elem 2, Elem 3]] :: NestedList Int) `shouldBe` [1, 2, 3]
    flatten (List [List [Elem 2, Elem 3], Elem 1] :: NestedList Int) `shouldBe` [2, 3, 1]
    flatten (List [Elem "a", List [Elem "b", List [Elem "c", Elem "d"], Elem "e"]] :: NestedList String) `shouldBe` ["a", "b", "c", "d", "e"]
    flatten (List [List [List [Elem "a"]]] :: NestedList String) `shouldBe` ["a"]

  it "[08] Should remove consecutive duplicates" $ do
    compress (["a", "a", "b", "c", "c"] :: [String]) `shouldBe` ["a", "b", "c"]
    compress (["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"] :: [String]) `shouldBe` ["a", "b", "c", "a", "d", "e"]
    compress ("aaaabccaadeeee" :: [Char]) `shouldBe` "abcade"

  it "[09] Should pack/combine duplicates" $ do
    pack ("aabcc" :: [Char]) `shouldBe` ["aa", "b", "cc"]
    pack ("aaaabccaadeeee" :: [Char]) `shouldBe` ["aaaa", "b", "cc", "aa", "d", "eeee"]

  it "[10] Should encode duplicates" $ do
    encode (['a', 'a', 'b', 'c', 'c'] :: [Char]) `shouldBe` [(2, 'a'), (1, 'b'), (2, 'c')]
    encode ("aabcc" :: [Char]) `shouldBe` [(2, 'a'), (1, 'b'), (2, 'c')]
    encode ([1, 1, 2, 3, 3] :: [Int]) `shouldBe` [(2, 1), (1, 2), (2, 3)]

  it "[11] Should encode duplicates but modified" $ do
    encodeModified ([1, 1, 2, 3, 3] :: [Int])
      `shouldBe` [ MultipleEncode 1 2,
                   SingleEncode 2,
                   MultipleEncode 3 2
                 ]
    encodeModified ("aaaabccaadeeee" :: [Char])
      `shouldBe` [ MultipleEncode 'a' 4,
                   SingleEncode 'b',
                   MultipleEncode 'c' 2,
                   MultipleEncode 'a' 2,
                   SingleEncode 'd',
                   MultipleEncode 'e' 4
                 ]

  it "[12] Should decode encoded duplicates" $ do
    decodeModified
      [ MultipleEncode 1 2,
        SingleEncode 2,
        MultipleEncode 3 2
      ]
      `shouldBe` ([1, 1, 2, 3, 3] :: [Int])
    decodeModified
      [ MultipleEncode 'a' 4,
        SingleEncode 'b',
        MultipleEncode 'c' 2,
        MultipleEncode 'a' 2,
        SingleEncode 'd',
        MultipleEncode 'e' 4
      ]
      `shouldBe` ("aaaabccaadeeee" :: [Char])

  it "[13] Should encode duplicates directly" $ do
    encodeDirect ([1, 1, 2, 3, 3] :: [Int])
      `shouldBe` [ MultipleEncode 1 2,
                   SingleEncode 2,
                   MultipleEncode 3 2
                 ]
    encodeDirect ("aaaabccaadeeee" :: [Char])
      `shouldBe` [ MultipleEncode 'a' 4,
                   SingleEncode 'b',
                   MultipleEncode 'c' 2,
                   MultipleEncode 'a' 2,
                   SingleEncode 'd',
                   MultipleEncode 'e' 4
                 ]

  it "[14] Should duplicate items in a list" $ do
    duplicate ("abccd" :: [Char]) `shouldBe` "aabbccccdd"
    duplicate ([1, 2, 3] :: [Int]) `shouldBe` [1, 1, 2, 2, 3, 3]

  it "[15] Should duplicate items in a list n times" $ do
    myReplicate ("abc" :: [Char]) 3 `shouldBe` "aaabbbccc"
    myReplicate ([1, 2, 3] :: [Int]) 3 `shouldBe` [1, 1, 1, 2, 2, 2, 3, 3, 3]

  it "[16] Should drop every nth item from a list" $ do
    dropEvery ([1 .. 7] :: [Int]) 2 `shouldBe` [1, 3, 5, 7]
    dropEvery ("abcdefghijk" :: [Char]) 3 `shouldBe` "abdeghjk"

  it "[17] Should split a given list into 2 parts" $ do
    split ([1 .. 10] :: [Int]) 0 `shouldBe` ([], [1 .. 10])
    split (['a' .. 'k'] :: [Char]) 3 `shouldBe` ("abc", "defghijk")

  it "[18] Should slice a list given a range" $ do
    slice ['a' .. 'k'] 3 7 `shouldBe` "cdefg"
    slice ['a' .. 'k'] 3 1 `shouldBe` ""
    slice ['a' .. 'k'] 3 3 `shouldBe` "c"

  it "[19] Should rotate a list" $ do
    rotate ['a' .. 'h'] 3 `shouldBe` "defghabc"
    rotate ['a' .. 'h'] (-2) `shouldBe` "ghabcdef"

  it "[20] Should remove nth element" $ do
    removeAt 2 ['a' .. 'd'] `shouldBe` "acd"
    removeAt (-1) ['a' .. 'd'] `shouldBe` "abcd"

  it "[21] Should insert at nth position" $ do
    insertAt 'X' ['a' .. 'd'] 2 `shouldBe` "aXbcd"
    insertAt 'X' ['a' .. 'd'] (-1) `shouldBe` "abcd"
    insertAt 'X' ['a' .. 'd'] (99) `shouldBe` "abcdX"

  it "[22] Should create an array with sequential elements given range" $ do
    range 4 9 `shouldBe` [4 .. 9]
    range 5 1 `shouldBe` []
    range 1 1 `shouldBe` [1]

  it "[23] Should get random selection" $ do
    rndSelect ([1 .. 10] :: [Int]) 3 >>= (`shouldBe` 3) . length
    rndSelect (['a' .. 'z'] :: [Char]) 5 >>= (`shouldBe` 5) . length

  it "[24] Should get random lotto selection" $ do
    lottoSelect 6 49 >>= (`shouldBe` 6) . length

  it "[25] Should generate random permutation" $ do
    rndPermutations (['a' .. 'f'] :: [Char]) >>= (`shouldBe` 6) . length
    rndPermutations ([1 .. 5] :: [Int]) >>= (`shouldBe` 5) . length

  it "[26] Should generate combinations" $ do
    combinations 0 (['a' .. 'd'] :: [Char]) `shouldBe` [[]]
    combinations 1 (['a' .. 'd'] :: [Char]) `shouldBe` [['a'], ['b'], ['c'], ['d']]
    combinations 1 ([1 .. 4] :: [Int]) `shouldBe` [[1], [2], [3], [4]]
    combinations 2 ([1 .. 4] :: [Int])
      `shouldBe` [ [1, 2],
                   [1, 3],
                   [1, 4],
                   [2, 3],
                   [2, 4],
                   [3, 4]
                 ]
    combinations 3 (['a' .. 'f'] :: [Char])
      `shouldBe` [ "abc",
                   "abd",
                   "abe",
                   "abf",
                   "acd",
                   "ace",
                   "acf",
                   "ade",
                   "adf",
                   "aef",
                   "bcd",
                   "bce",
                   "bcf",
                   "bde",
                   "bdf",
                   "bef",
                   "cde",
                   "cdf",
                   "cef",
                   "def"
                 ]

  it "[27] Should return combinations of length 3" $ do
    ( length $
        group3
          ( [ "aldo",
              "beat",
              "carla",
              "david",
              "evi",
              "flip",
              "gary",
              "hugo",
              "ida"
            ] ::
              [String]
          )
      )
      `shouldBe` 84
    (length $ group3 ([1 .. 6] :: [Int])) `shouldBe` 20
    group3 ([1 .. 4] :: [Int])
      `shouldBe` [ [[1, 2, 3]],
                   [[1, 2, 4]],
                   [[1, 3, 4]],
                   [[2, 3, 4]]
                 ]

  it "[28] Should handle a generalized `group3`" $ do
    ( length $
        group [2, 3, 4] ["aldo", "beat", "carla", "david", "evi", "flip", "gary", "hugo", "ida"]
      )
      `shouldBe` 1260
    ( length $
        group [2, 2, 5] ["aldo", "beat", "carla", "david", "evi", "flip", "gary", "hugo", "ida"]
      )
      `shouldBe` 756
    group [2, 1] ["a", "b", "c", "d"]
      `shouldBe` [ [["a", "b"], ["c"]],
                   [["a", "b"], ["d"]],
                   [["a", "c"], ["b"]],
                   [["a", "c"], ["d"]],
                   [["a", "d"], ["b"]],
                   [["a", "d"], ["c"]],
                   [["b", "c"], ["a"]],
                   [["b", "c"], ["d"]],
                   [["b", "d"], ["a"]],
                   [["b", "d"], ["c"]],
                   [["c", "d"], ["a"]],
                   [["c", "d"], ["b"]]
                 ]

  it "[29] Should return elements sorted by length" $ do
    lsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"] `shouldBe` ["o", "de", "de", "mn", "abc", "fgh", "ijkl"]

  it "[30] Should return elements sorted by least frequency lengths first" $ do
    lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"] `shouldBe` ["ijkl", "o", "abc", "fgh", "de", "de", "mn"]

  it "[31] Should return true if given number is prime" $ do
    isPrime (0 :: Integer) `shouldBe` False
    isPrime (4 :: Integer) `shouldBe` False
    isPrime (7 :: Integer) `shouldBe` True
    isPrime (17 :: Integer) `shouldBe` True
    isPrime (47 :: Integer) `shouldBe` True
    isPrime (223 :: Integer) `shouldBe` True

  it "[32] Should calculate the prime factors" $ do
    primeFactors 315 `shouldBe` [3, 3, 5, 7]
    primeFactors 35 `shouldBe` [5, 7]
    primeFactors 820 `shouldBe` [2, 2, 5, 41]

  it "[33] Should calculate the prime factors and multiplicities" $ do
    primeFactorsMult 315
      `shouldBe` [ (3, 2),
                   (5, 1),
                   (7, 1)
                 ]
    primeFactorsMult 820
      `shouldBe` [ (2, 2),
                   (5, 1),
                   (41, 1)
                 ]

  it "[34] Should return a list of primes within a range" $ do
    primesFrom 10 20 `shouldBe` [11, 13, 17, 19]
    primesFrom 50 100 `shouldBe` [53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
    length (primesFrom 2 7920) `shouldBe` 1000

  it "[35] Should return two primes that sum to the given even number" $ do
    unsafeGoldbach 28 `shouldBe` (5, 23)
    unsafeGoldbach 60 `shouldBe` (7, 53)

  it "[36] Should return a list of all even numbers and their Goldbach composition" $ do
    unsafeGoldbachList 9 20
      `shouldBe` [ (10, (3, 7)),
                   (12, (5, 7)),
                   (14, (3, 11)),
                   (16, (3, 13)),
                   (18, (5, 13)),
                   (20, (3, 17))
                 ]
    length (unsafeGoldbachList 3 3000) `shouldBe` 1499

  it "[37] Should return the gcd of two numbers" $ do
    myGCD 36 63 `shouldBe` 9
    myGCD (-3) (-6) `shouldBe` 3
    myGCD (-3) 6 `shouldBe` 3
    myGCD 234 42 `shouldBe` 6

  it "[38] Should return true if two numbers are coprime" $ do
    coprime 35 64 `shouldBe` True
    coprime 15 8 `shouldBe` True
    coprime 3 20 `shouldBe` True
    coprime 5 12 `shouldBe` True

  it "[39] Should return the totient" $ do
    totientPhi 10 `shouldBe` 4
    totientPhi 13 `shouldBe` 12
    totientPhi 9 `shouldBe` 6
    totientPhi 20 `shouldBe` 8

  it "[40] Should return the totient using the improved method" $ do
    phi 10 `shouldBe` 4
    phi 13 `shouldBe` 12
    phi 9 `shouldBe` 6
    phi 20 `shouldBe` 8

  it "[41] Should time both phi functions" $ do
    True `shouldBe` True

  it "[42] Should return a boolean table" $ do
    -- a ∧ (a ∨ b)
    table (\a b -> (and' a (or' a b)))
      `shouldBe` TruthTable
        ( TTRow (True, True, True),
          TTRow (True, False, True),
          TTRow (False, True, False),
          TTRow (False, False, False)
        )
    -- (a ∨ b) ∧ (a ∧ b)
    table (\a b -> (and' (or' a b) (and' a b)))
      `shouldBe` TruthTable
        ( TTRow (True, True, True),
          TTRow (True, False, False),
          TTRow (False, True, False),
          TTRow (False, False, False)
        )

  xit "[43]" $ do
    True `shouldBe` True

  xit "[44]" $ do
    True `shouldBe` True

  it "[45] Should return gray codes" $ do
    gray 1 `shouldBe` ["0", "1"]
    gray 2 `shouldBe` ["00", "01", "11", "10"]
    gray 3 `shouldBe` ["000", "001", "011", "010", "110", "111", "101", "100"]

  xit "[46] Should return huffman code table" $ do
    huffman
      [ ("a", 45),
        ("b", 13),
        ("c", 12),
        ("d", 16),
        ("e", 9),
        ("f", 5)
      ]
      `shouldBe` [ ("a", "0"),
                   ("c", "100"),
                   ("b", "101"),
                   ("f", "1100"),
                   ("e", "1101"),
                   ("d", "111")
                 ]

  it "[47] Should return true if given a valid binary tree" $ do
    let tree :: BinaryTree Int
        tree = Branch 1 Empty Empty
     in isBinaryTree tree `shouldBe` True

  xit "[48]" $ do
    True `shouldBe` True

  it "[49] Should return a symmetric binary tree" $ do
    let t1 :: BinaryTree Char
        --     x
        --    / \
        --   x   x
        --        \
        --         x
        t1 = Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))
        t2 :: BinaryTree Char
        --     x
        --    / \
        --   x   x
        --  /     \
        -- x       x
        t2 = Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))
        t3 :: BinaryTree Char
        --      x
        --    /  \
        --   x    x
        --    \  /
        --    x x
        t3 = Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty)
     in [isSymmetricTree t1, isSymmetricTree t2, isSymmetricTree t3] `shouldBe` [False, True, True]
