module Main where

import Test.Hspec
import qualified Test.Combinatron.Types as Types

main = hspec spec

spec :: Spec
spec = describe "Test" $ do
    Types.spec
