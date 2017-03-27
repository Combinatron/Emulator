module Test.Combinatron.Types (
  spec
) where

import qualified Combinatron.Types as Combinatron
import Test.Hspec
import Test.QuickCheck
import Test.Combinatron.QuickCheck
import Control.Lens (view)
import qualified Data.Vector as V

spec = do
    describe "Type tests" $ do
        initialize

initialize :: Spec
initialize = do
    it "does not set the middle cursor" $ property $
        \ i -> view Combinatron.midCursor (Combinatron.initialize i) == Combinatron.emptyCursor
    it "does not set the top cursor" $ property $
        \ i -> view Combinatron.topCursor (Combinatron.initialize i) == Combinatron.emptyCursor
    it "does not set the value" $ property $
        \ i -> view Combinatron.value (Combinatron.initialize i) == Combinatron.emptySentence
    it "bottom cursor contains the first sentence in the program" $ forAll (arbitrary :: Gen NonEmptySentenceIndex) $
        \ (NonEmptySentenceIndex i) ->
            all ($ Combinatron.initialize i)
                [ (== V.head i) . view (Combinatron.botCursor . Combinatron.cursorSentence)
                , (== Combinatron.newPointer 1) . view (Combinatron.botCursor . Combinatron.cursorPointer)
                ]
    it "puts the program in the index" $ property $
        \ i -> view Combinatron.sentenceIndex (Combinatron.initialize i) == i
