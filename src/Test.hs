module Main where

import Test.Hspec
import Test.QuickCheck

main = hspec spec

spec :: Spec
spec = describe "Test" $ do
    it "returns the first element of an *arbitrary* list" $
        property $ \x xs -> head (x:xs) == (x :: Int)
