module MyLibSpec (spec) where

import MyLib (myLast)
import Test.Hspec

spec :: Spec
spec = do
  it "[01] Should return the last element of a list" $ do
    myLast [] `shouldBe` (Nothing :: Maybe ())
    myLast ["a"] `shouldBe` (Just "a" :: Maybe String)
    myLast [1, 2, 3] `shouldBe` (Just 3 :: Maybe Int)
