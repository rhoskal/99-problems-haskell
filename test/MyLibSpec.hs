module MyLibSpec (spec) where

import MyLib (elementAt, lastTwo, myLast)
import Test.Hspec

spec :: Spec
spec = do
  it "[01] Should return the last element of a list" $ do
    myLast [] `shouldBe` (Nothing :: Maybe ())
    myLast ["a"] `shouldBe` (Just "a" :: Maybe String)
    myLast [1, 2, 3] `shouldBe` (Just 3 :: Maybe Int)

  it "[02] Should return the last two elements of a list" $ do
    lastTwo [] `shouldBe` (Nothing :: Maybe [()])
    lastTwo [True] `shouldBe` (Nothing :: Maybe [Bool])
    lastTwo [1, 2, 3] `shouldBe` (Just [2, 3] :: Maybe [Int])

  it "[03] Should return the nth element of a list" $ do
    elementAt 2 [] `shouldBe` (Nothing :: Maybe ())
    elementAt (-2) [] `shouldBe` (Nothing :: Maybe ())
    elementAt 2 [1, 2] `shouldBe` (Just 2 :: Maybe Int)
    elementAt 2 ["a", "b", "c", "d", "e"] `shouldBe` (Just "b" :: Maybe String)
