{-# LANGUAGE RankNTypes #-}
module Test.Combinatron.Operations (
  spec
) where

import qualified Combinatron.Types as Combinatron
import qualified Combinatron.Operations as Ops
import Test.Hspec
import Test.QuickCheck
import Test.Combinatron.QuickCheck
import Control.Lens (view, Lens')
import qualified Data.Vector as V

spec = do
    fetchCursor

makePointer :: Combinatron.SentenceIndex -> Positive Int -> Combinatron.Pointer
makePointer program (Positive p) = Combinatron.newPointer $ (p `mod` V.length program) + 1

testData :: Combinatron.SentenceIndex -> Positive Int -> Lens' Combinatron.Machine Combinatron.Cursor -> (Combinatron.Pointer, Combinatron.Machine, Combinatron.Machine, Combinatron.SentenceIndex)
testData program p cursor = (pointer, machine, newMachine, newProgram)
    where
        pointer = makePointer program p
        machine = Combinatron.initialize program
        newMachine = Ops.fetchCursor pointer cursor machine
        newProgram = view Combinatron.sentenceIndex newMachine

-- | Some of these specs require the generated program to be non empty. This is
-- because I use its length as the second argument to a `mod` operation and a
-- zero length results in a divide by zero error. I need that so that the
-- arbitrary generated pointers are to indexes within the generated program. So
-- a better generation of thoese means the mod can be dispensed with and thus
-- the non-empty requirement.
fetchCursor :: Spec
fetchCursor = do
    describe "fetchCursor" $ do
        it "does not modify the SentenceIndex" $ property $
            -- The reason a NonEmptySentenceIndex isn't required here is
            -- because of laziness. It's required in other tests because an
            -- empty sentence index, when passed to makePointer, will throw a
            -- divide by zero error. In this test the pointer is used in
            -- fetchCursor and, crucially, the fetched cursor is _never used_.
            -- Because of this the pointer is never forced and the division is
            -- never evaluated. Adding `&& view cursor newMachine == Combinatron.emptyCursor`
            -- to the end of the conditional will force the pointer to be
            -- evaluated. I'm not sure if this is indicative of a problem. I
            -- think that if the `fetchCursor` function modified the sentence
            -- indexes it would require forcing the pointer, so I don't think
            -- it is a problem.
            \ oldProgram (CursorSelection _ cursor) p ->
                let (_, _, _, newProgram) = testData oldProgram p cursor
                in oldProgram == newProgram

        it "only modifies the specified cursor or does not modify anything" $ property $
            \ (NonEmptySentenceIndex oldProgram) (CursorSelection _ cursor) p ->
                let (_, machine, newMachine, _) = testData oldProgram p cursor
                    numModified = length $ filter id $ map (\ c -> view c newMachine /= view c machine) [Combinatron.botCursor, Combinatron.midCursor, Combinatron.topCursor]
                in (numModified == 1 && view cursor newMachine /= view cursor machine) || numModified == 0

        it "the specified cursor must contain a sentence that is in the sentence index or it is null" $ property $
            \ (NonEmptySentenceIndex oldProgram) (CursorSelection _ cursor) p ->
                let (_, _, newMachine, _) = testData oldProgram p cursor
                in V.elem (view (cursor . Combinatron.cursorSentence) newMachine) oldProgram || (view cursor newMachine == Combinatron.emptyCursor)

        it "the specified cursor must contain the sentence in the index that it points to" $ property $
            \ (NonEmptySentenceIndex oldProgram) (CursorSelection _ cursor) p ->
                let (_, _, newMachine, _) = testData oldProgram p cursor
                    cp = view (cursor . Combinatron.cursorPointer) newMachine
                    cs = view (cursor . Combinatron.cursorSentence) newMachine
                    ms = Combinatron.usePointer cp Nothing ((V.!?) oldProgram)
                in maybe False (\ s -> s == cs) ms || view cursor newMachine == Combinatron.emptyCursor

        it "the specified cursor must point to the same pointer as requested" $ property $
            \ (NonEmptySentenceIndex oldProgram) (CursorSelection _ cursor) p ->
                let (pointer, _, newMachine, _) = testData oldProgram p cursor
                    cp = view (cursor . Combinatron.cursorPointer) newMachine
                in cp == pointer || view cursor newMachine == Combinatron.emptyCursor
