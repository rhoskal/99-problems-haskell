module MyLibSpec (spec) where

import MyLib
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
    insertAt,
    isPalindrome,
    lastTwo,
    lottoSelect,
    myLast,
    myLength,
    myReplicate,
    myReverse,
    pack,
    range,
    removeAt,
    rndSelect,
    rotate,
    slice,
    split,
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
    elementAt 2 [] `shouldBe` (Nothing :: Maybe Int)
    elementAt (-2) [1, 2] `shouldBe` (Nothing :: Maybe Int)
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

  it "[24] Should get random selection" $ do
    lottoSelect 6 49 >>= (`shouldBe` 6) . length
