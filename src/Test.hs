module Main where

import Test.Hspec
import qualified Test.Combinatron.Types as Types
import qualified Test.Combinatron.Operations as Operations
import qualified Test.Combinatron.Files as Files

main = hspec spec

spec :: Spec
spec = describe "Test" $ do
    Types.spec
    Operations.spec
    Files.spec
