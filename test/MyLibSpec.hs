module MyLibSpec (spec) where

import MyLib
  ( NestedList (..),
    compress,
    elementAt,
    flatten,
    isPalindrome,
    lastTwo,
    myLast,
    myLength,
    myReverse,
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
