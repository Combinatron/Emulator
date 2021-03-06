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
    writeCursor
    addSentenceAndUpdate
    newMWord
    newNWord
    swapWords
    copyWord
    zeroWord
    swapCursors
    putValue
    getValue
    conditionals

doesNotModifySentenceIndex :: Combinatron.Machine -> (Combinatron.Machine -> Combinatron.Machine) -> Bool
doesNotModifySentenceIndex oldMachine op = oldProgram == newProgram
    where
        oldProgram = view Combinatron.sentenceIndex oldMachine
        newMachine = op oldMachine
        newProgram = view Combinatron.sentenceIndex newMachine

doesNotModifyCursors :: Combinatron.Machine -> (Combinatron.Machine -> Combinatron.Machine) -> Bool
doesNotModifyCursors oldMachine op = oldCursors == newCursors
    where
        newMachine = op oldMachine
        oldCursors = map (flip view oldMachine) [Combinatron.botCursor, Combinatron.midCursor, Combinatron.topCursor]
        newCursors = map (flip view newMachine) [Combinatron.botCursor, Combinatron.midCursor, Combinatron.topCursor]

makePointer :: Combinatron.SentenceIndex -> Positive Int -> Combinatron.Pointer
makePointer program (Positive p) = Combinatron.newPointer $ (p `mod` V.length program) + 1

numberOfModifiedWords oldMachine newMachine = numModified
    where
        numModified = length $ filter id $ concatMap sentenceModified [Combinatron.botCursor, Combinatron.midCursor, Combinatron.topCursor]
        sentenceModified s = map (\ w -> view (s . Combinatron.cursorSentence . w) newMachine /= view (s . Combinatron.cursorSentence . w) oldMachine) [Combinatron.priWord, Combinatron.secWord, Combinatron.triWord]

numberOfModifiedCursors oldMachine newMachine = numModified
    where
        numModified = length $ filter id $ map (\ c -> view c newMachine /= view c oldMachine) [Combinatron.botCursor, Combinatron.midCursor, Combinatron.topCursor]

numberOfModifiedSentences oldProgram newProgram = numModified
    where
        oldNew = V.zipWith (/=) oldProgram newProgram
        numModified = V.length $ V.filter id oldNew

fetchCursorTestData :: Combinatron.SentenceIndex -> Positive Int -> Lens' Combinatron.Machine Combinatron.Cursor -> (Combinatron.Pointer, Combinatron.Machine, Combinatron.Machine, Combinatron.SentenceIndex)
fetchCursorTestData program p cursor = (pointer, machine, newMachine, newProgram)
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
            -- TODO: refactor to use doesNotModifySentenceIndex
            \ oldProgram (CursorSelection _ cursor) p ->
                let (_, _, _, newProgram) = fetchCursorTestData oldProgram p cursor
                in oldProgram == newProgram

        it "only modifies the specified cursor or does not modify anything" $ property $
            \ (NonEmptySentenceIndex oldProgram) (CursorSelection _ cursor) p ->
                let (_, machine, newMachine, _) = fetchCursorTestData oldProgram p cursor
                    numModified = numberOfModifiedCursors machine newMachine
                in (numModified == 1 && view cursor newMachine /= view cursor machine) || numModified == 0

        it "the specified cursor must contain a sentence that is in the sentence index or it is null" $ property $
            \ (NonEmptySentenceIndex oldProgram) (CursorSelection _ cursor) p ->
                let (_, _, newMachine, _) = fetchCursorTestData oldProgram p cursor
                in V.elem (view (cursor . Combinatron.cursorSentence) newMachine) oldProgram || (view cursor newMachine == Combinatron.emptyCursor)

        it "the specified cursor must contain the sentence in the index that it points to" $ property $
            \ (NonEmptySentenceIndex oldProgram) (CursorSelection _ cursor) p ->
                let (_, _, newMachine, _) = fetchCursorTestData oldProgram p cursor
                    cp = view (cursor . Combinatron.cursorPointer) newMachine
                    cs = view (cursor . Combinatron.cursorSentence) newMachine
                    ms = Combinatron.usePointer cp Nothing ((V.!?) oldProgram)
                in maybe False (\ s -> s == cs) ms || view cursor newMachine == Combinatron.emptyCursor

        it "the specified cursor must point to the same pointer as requested" $ property $
            \ (NonEmptySentenceIndex oldProgram) (CursorSelection _ cursor) p ->
                let (pointer, _, newMachine, _) = fetchCursorTestData oldProgram p cursor
                    cp = view (cursor . Combinatron.cursorPointer) newMachine
                in cp == pointer || view cursor newMachine == Combinatron.emptyCursor

writeCursorTestData :: Combinatron.Machine -> Lens' Combinatron.Machine Combinatron.Cursor -> (Combinatron.Machine, Combinatron.SentenceIndex, Combinatron.SentenceIndex)
writeCursorTestData oldMachine cursor = (newMachine, oldProgram, newProgram)
    where
        oldProgram = view Combinatron.sentenceIndex oldMachine
        newMachine = Ops.writeCursor cursor oldMachine
        newProgram = view Combinatron.sentenceIndex newMachine

-- The SteppedMachine generator makes these tests more robust as there's a
-- higher likelihood that the selected cursor will be filled.
writeCursor :: Spec
writeCursor = do
    describe "writeCursor" $ do
        it "does not change the size of the index" $ property $
            \ (SteppedMachine oldMachine) (CursorSelection _ cursor) ->
                let (_, oldProgram, newProgram) = writeCursorTestData oldMachine cursor
                in V.length oldProgram == V.length newProgram

        it "does not modify the cursors" $ property $
            \ (SteppedMachine oldMachine) (CursorSelection _ cursor) ->
                let (newMachine, _, _) = writeCursorTestData oldMachine cursor
                    numModified = numberOfModifiedCursors oldMachine newMachine
                in numModified == 0

        it "the sentence at the location of the cursor in the index must be the same as the cursor" $ property $
            \ (SteppedMachine oldMachine) (CursorSelection _ cursor) ->
                let (newMachine, _, newProgram) = writeCursorTestData oldMachine cursor
                    cp = view (cursor . Combinatron.cursorPointer) newMachine
                    cs = view (cursor . Combinatron.cursorSentence) newMachine
                    progS = Combinatron.usePointer cp Combinatron.emptySentence ((V.!) newProgram)
                in cs == progS

        it "does not modify any location other than the pointed one" $ property $
            \ (SteppedMachine oldMachine) (CursorSelection _ cursor) ->
                let (newMachine, oldProgram, newProgram) = writeCursorTestData oldMachine cursor
                    cp = view (cursor . Combinatron.cursorPointer) newMachine
                    numModified = numberOfModifiedSentences oldProgram newProgram
                in (numModified == 1 && Combinatron.usePointer cp True (\i -> oldProgram V.! i /= newProgram V.! i)) || numModified == 0

addSentenceAndUpdateTestData :: Combinatron.SentenceIndex -> Combinatron.Sentence -> Lens' Combinatron.Machine Combinatron.Word -> (Combinatron.SentenceIndex, Combinatron.Machine, Combinatron.Machine)
addSentenceAndUpdateTestData prog s cw = (newProgram, newMachine, oldMachine)
    where
        oldMachine = Combinatron.initialize prog
        newMachine = Ops.addSentenceAndUpdate s cw oldMachine
        newProgram = view Combinatron.sentenceIndex newMachine

addSentenceAndUpdate :: Spec
addSentenceAndUpdate = do
    describe "addSentenceAndUpdate" $ do
        it "increases the index size by 1" $ property $
            \ oldProgram cwsel newSentence ->
                let (newProgram, _, _) = addSentenceAndUpdateTestData oldProgram newSentence (toLens cwsel)
                in V.length newProgram == 1 + V.length oldProgram

        it "does not modify any existing portion of the index" $ property $
            \ oldProgram cwsel newSentence ->
                let (newProgram, _, _) = addSentenceAndUpdateTestData oldProgram newSentence (toLens cwsel)
                    allIn = V.all (flip V.elem newProgram) oldProgram
                in allIn

        it "the sentence index must now contain the sentence passed in" $ property $
            \ oldProgram cwsel newSentence ->
                let (newProgram, _, _) = addSentenceAndUpdateTestData oldProgram newSentence (toLens cwsel)
                in V.elem newSentence newProgram

        it "only modifies the given word" $ property $
            \ (ValidProgram oldProgram) cwsel newSentence ->
                let (_, newMachine, oldMachine) = addSentenceAndUpdateTestData oldProgram newSentence (toLens cwsel)
                    numModified = numberOfModifiedWords oldMachine newMachine
                in numModified == 1 && (view (toLens cwsel) newMachine /= view (toLens cwsel) oldMachine)

newMWord :: Spec
newMWord = do
    describe "newMWord" $ do
        it "does not modify the sentence index" $ property $
            \ (SteppedMachine m) -> doesNotModifySentenceIndex m Ops.newMWord

        it "does not modify the top or bottom cursors" $ property $
            \ (SteppedMachine m) ->
                let newMachine = Ops.newMWord m
                in (view Combinatron.topCursor m == view Combinatron.topCursor newMachine) && (view Combinatron.botCursor m == view Combinatron.botCursor newMachine)

        it "the first word in the middle cursor must be an M" $ property $
            \ (SteppedMachine m) ->
                let newMachine = Ops.newMWord m
                in Combinatron.isM (view (Combinatron.midCursor . Combinatron.cursorSentence . Combinatron.priWord) newMachine)

        it "the first word in the middle cursor must point to the location of the top cursor" $ property $
            \ (SteppedMachine m) ->
                let newMachine = Ops.newMWord m
                    tp = view (Combinatron.topCursor . Combinatron.cursorPointer) newMachine
                    (Combinatron.M p) = view (Combinatron.midCursor . Combinatron.cursorSentence . Combinatron.priWord) newMachine
                in p == tp

newNWord :: Spec
newNWord = do
    describe "newNWord" $ do
        it "does not modify the sentence index" $ property $
            \ (SteppedMachine m) -> doesNotModifySentenceIndex m Ops.newNWord

        it "does not modify the top or bottom cursors" $ property $
            \ (SteppedMachine m) ->
                let newMachine = Ops.newNWord m
                in (view Combinatron.topCursor m == view Combinatron.topCursor newMachine) && (view Combinatron.botCursor m == view Combinatron.botCursor newMachine)

        it "the first word in the middle cursor must be an N" $ property $
            \ (SteppedMachine m) ->
                let newMachine = Ops.newNWord m
                in Combinatron.isN (view (Combinatron.midCursor . Combinatron.cursorSentence . Combinatron.priWord) newMachine)

        it "the first word in the middle cursor must point to the location of the bottom cursor" $ property $
            \ (SteppedMachine m) ->
                let newMachine = Ops.newNWord m
                    bp = view (Combinatron.botCursor . Combinatron.cursorPointer) newMachine
                    (Combinatron.N p) = view (Combinatron.midCursor . Combinatron.cursorSentence . Combinatron.priWord) newMachine
                in p == bp

swapWords :: Spec
swapWords = do
    describe "swapWords" $ do
        it "does not modify the sentence index" $ property $
            \ (SteppedMachine m) cw1 cw2 -> doesNotModifySentenceIndex m (Ops.swapWords (toLens cw1) (toLens cw2))

        it "only modifies the two words or modifies no words" $ property $
            \ (SteppedMachine oldMachine) cw1 cw2 ->
                let newMachine = Ops.swapWords (toLens cw1) (toLens cw2) oldMachine
                    numModified = numberOfModifiedWords oldMachine newMachine
                in numModified == 0 || (numModified == 2 && view (toLens cw1) oldMachine /= view (toLens cw1) newMachine && view (toLens cw2) oldMachine /= view (toLens cw2) newMachine)

        it "the two words are swapped" $ property $ do
            \ (SteppedMachine oldMachine) cw1 cw2 ->
                let newMachine = Ops.swapWords (toLens cw1) (toLens cw2) oldMachine
                    old = \ cw -> view (toLens cw) oldMachine
                    new = \ cw -> view (toLens cw) newMachine
                in old cw1 == new cw2 && old cw2 == new cw1

copyWord :: Spec
copyWord = do
    describe "copyWord" $ do
        it "does not modify the sentence index" $ property $
            \ (SteppedMachine m) cw1 cw2 -> doesNotModifySentenceIndex m (Ops.copyWord (toLens cw1) (toLens cw2))

        it "only modifies the second word argument" $ property $
            \ (SteppedMachine m) notModified modified ->
                let newMachine = Ops.copyWord (toLens notModified) (toLens modified) m
                    numModified = numberOfModifiedWords m newMachine
                in numModified == 0 || (numModified == 1 && view (toLens modified) m /= view (toLens modified) newMachine)
        it "the second word is the same as the first word" $ property $
            \ (SteppedMachine m) notModified modified ->
                let newMachine = Ops.copyWord (toLens notModified) (toLens modified) m
                in view (toLens notModified) newMachine == view (toLens modified) newMachine

zeroWord :: Spec
zeroWord = do
    describe "zeroWord" $ do
        it "does not modify the sentence index" $ property $
            \ (SteppedMachine m) cw -> doesNotModifySentenceIndex m (Ops.zeroWord (toLens cw))

        it "only modifies the pointed word" $ property $
            \ (SteppedMachine m) cw ->
                let newMachine = Ops.zeroWord (toLens cw) m
                    numModified = numberOfModifiedWords m newMachine
                in numModified == 0 || numModified == 1 && view (toLens cw) m /= view (toLens cw) newMachine

        it "zero's the pointed word" $ property $
            \ (SteppedMachine m) cw ->
                let newMachine = Ops.zeroWord (toLens cw) m
                in view (toLens cw) newMachine == Combinatron.NullWord

swapCursors :: Spec
swapCursors = do
    describe "swapCursors" $ do
        it "does not modify the sentence index" $ property $
            \ (SteppedMachine m) (CursorSelection _ c1) (CursorSelection _ c2) -> doesNotModifySentenceIndex m (Ops.swapCursors c1 c2)

        it "swaps the cursors" $ property $
            \ (SteppedMachine m) (CursorSelection _ c1) (CursorSelection _ c2) ->
                let newMachine = Ops.swapCursors c1 c2 m
                    oldC1 = view c1 m
                    oldC2 = view c2 m
                    newC1 = view c1 newMachine
                    newC2 = view c2 newMachine
                in oldC1 == newC2 && oldC2 == newC1

        it "does not modify non-pointed cursor" $ property $
            \ (SteppedMachine m) (CursorSelection _ c1) (CursorSelection _ c2) ->
                let newMachine = Ops.swapCursors c1 c2 m
                    numModified = numberOfModifiedCursors m newMachine
                    oldC1 = view c1 m
                    oldC2 = view c2 m
                    newC1 = view c1 newMachine
                    newC2 = view c2 newMachine
                in numModified == 0 || numModified == 2 && oldC1 /= newC1 && oldC2 /= newC2

putValue :: Spec
putValue = do
    describe "putValue" $ do
        it "does not modify the cursors" $ property $
            \ (MachineWithValue m) p -> doesNotModifyCursors m (Ops.putValue (makePointer (view Combinatron.sentenceIndex m) p))

        it "does not modify the value" $ property $
            \ (MachineWithValue m) p ->
                let newMachine = Ops.putValue pointer m
                    pointer = makePointer oldProgram p
                    oldProgram = view Combinatron.sentenceIndex m
                    oldV = view Combinatron.value m
                    newV = view Combinatron.value newMachine
                in oldV == newV

        it "the sentence at the location pointed to in the index must be the same as the value" $ property $
            \ (MachineWithValue m) p ->
                let newMachine = Ops.putValue pointer m
                    pointer = makePointer oldProgram p
                    newProgram = view Combinatron.sentenceIndex newMachine
                    oldProgram = view Combinatron.sentenceIndex m
                    v = view Combinatron.value newMachine
                in Combinatron.usePointer pointer False (\ i -> maybe False id $ ((==) v) <$> newProgram V.!? i)

        it "does not modify any location other than the pointed one" $ property $
            \ (MachineWithValue m) p ->
                let newMachine = Ops.putValue pointer m
                    pointer = makePointer oldProgram p
                    newProgram = view Combinatron.sentenceIndex newMachine
                    oldProgram = view Combinatron.sentenceIndex m
                    numModified = numberOfModifiedSentences oldProgram newProgram
                in (numModified == 1 && Combinatron.usePointer pointer True (\i -> oldProgram V.! i /= newProgram V.! i)) || numModified == 0

getValue :: Spec
getValue = do
    describe "getValue" $ do
        it "does not modify the cursors" $ property $
            \ (MachineWithValue m) p -> doesNotModifyCursors m (Ops.getValue (makePointer (view Combinatron.sentenceIndex m) p))

        it "does not modify the sentence index" $ property $
            \ (MachineWithValue m) p -> doesNotModifySentenceIndex m (Ops.getValue (makePointer (view Combinatron.sentenceIndex m) p))

        it "the value is the same as the location pointed to in the sentence index" $ property $
            \ (MachineWithValue m) p ->
                let newMachine = Ops.getValue pointer m
                    pointer = makePointer oldProgram p
                    oldProgram = view Combinatron.sentenceIndex m
                    newV = view Combinatron.value newMachine
                in Combinatron.usePointer pointer False (\ i -> oldProgram V.! i == newV)

conditionals :: Spec
conditionals = do
    describe "strict conditionals (noWord, oneWord, twoWord, threeWord)" $ do
        it "only one returns true for a given cursor" $ property $
            \ (SteppedMachine m) ->
                let num = length . filter id . map ($ m) $ [Ops.noWord Combinatron.botCursor, Ops.oneWord Combinatron.botCursor, Ops.twoWord Combinatron.botCursor, Ops.threeWord Combinatron.botCursor]
                in num == 1

    describe "lax conditionals (noWord, oneWord', twoWord')" $ do
        it "subsume each other, noWord or oneWord' or oneWord' and twoWord'" $ property $
            \ (SteppedMachine m) ->
                let no = Ops.noWord Combinatron.botCursor m
                    one = Ops.oneWord' Combinatron.botCursor m
                    two = Ops.twoWord' Combinatron.botCursor m
                in no || one || (one && two)

    describe "strict and lax conditionals" $ do
        it "oneWord' is at least one word" $ property $
            \ (SteppedMachine m) ->
                let no = Ops.noWord Combinatron.botCursor m
                    one' = Ops.oneWord' Combinatron.botCursor m
                    one = Ops.oneWord Combinatron.botCursor m
                    two = Ops.twoWord Combinatron.botCursor m
                    three = Ops.threeWord Combinatron.botCursor m
                in no || one && one' || two && one' || three && one'

        it "twoWord' is at least two words" $ property $
            \ (SteppedMachine m) ->
                let no = Ops.noWord Combinatron.botCursor m
                    one = Ops.oneWord Combinatron.botCursor m
                    two = Ops.twoWord Combinatron.botCursor m
                    two' = Ops.twoWord' Combinatron.botCursor m
                    three = Ops.threeWord Combinatron.botCursor m
                in no || one && (not two') || two && two' || three && two'
