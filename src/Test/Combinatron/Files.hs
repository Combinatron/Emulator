module Test.Combinatron.Files (
  spec
) where

import Test.Hspec
import Test.QuickCheck (property)
import Test.Combinatron.QuickCheck()
import Combinatron.Assembler as A
import Combinatron.Loader as L

spec :: Spec
spec = do
    describe "assembling and loading files" $ do
        it "load . assemble $ prog == prog" $ property $
            \ program -> program == (L.loadFile . A.assembleFile $ program)
