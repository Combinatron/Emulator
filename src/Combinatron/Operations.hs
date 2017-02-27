{-# LANGUAGE Rank2Types #-}
module Combinatron.Operations (
  fetchCursor,
  writeCursor,
  addSentence,
  newMWord,
  newNWord,
  swapWords,
  copyWord,
  zeroWord,
  swapCursors,
  rotateCursorsUp,
  rotateCursorsDown,
  noWord, oneWord, twoWord, threeWord, oneWord', twoWord',
  c0w0, c0w1, c0w2, c1w0, c1w1, c1w2, c2w0, c2w1, c2w2
) where
{-
 - A library of primitive operations to modify Machine state.
 -}

import Prelude hiding (Word)
import Combinatron.Types
import qualified Data.Vector as V
import Control.Lens

-- | Read a sentence from the index into a cursor
fetchCursor :: Pointer -> Lens' Machine Cursor -> Machine -> Machine
fetchCursor p c m = set c (cursorAt p (m^.sentenceIndex)) m

-- | Write a cursor to the index
writeCursor :: Lens' Machine Cursor -> Machine -> Machine
writeCursor c m = m'
    where
        p = m^.c.cursorPointer
        sentence = m^.c.cursorSentence
        m' = usePointer p m (\x -> m & sentenceIndex %~ (\i -> i V.// [(pred x, sentence)]))

-- | Add a sentence to the index, writing an N word to a specified location.
addSentence :: Sentence -> Lens' Machine Word -> Machine -> Machine
addSentence s w m = set w newPointer (set sentenceIndex sI m)
    where
        sI = V.snoc (m^.sentenceIndex) s
        newPointer = n (succ $ V.length $ m^.sentenceIndex)

-- | Create a new M word pointing to the location of the top cursor
newMWord :: Machine -> Machine
newMWord m = set (midCursor.cursorSentence.priWord) p m
    where
        p = M $ m^.topCursor.cursorPointer

-- | Create a new N word pointing to the location of the bottom cursor
newNWord :: Machine -> Machine
newNWord m = set (midCursor.cursorSentence.priWord) p m
    where
        p = N $ m^.botCursor.cursorPointer

-- | Swap two words. They don't need to be in the same cursor.
swapWords :: Lens' Machine Word -> Lens' Machine Word -> Machine -> Machine
swapWords a b m = set b (m^.a) $ (set a (m^.b) m)

-- | Copy word a to word b. They don't need to be in the same cursor.
copyWord :: Lens' Machine Word -> Lens' Machine Word -> Machine -> Machine
copyWord a b m = set b (m^.a) m

-- | Write a NullWord to a given word location.
zeroWord :: Lens' Machine Word -> Machine -> Machine
zeroWord w m = set w NullWord m

-- | Swap two cursors.
swapCursors :: Lens' Machine Cursor -> Lens' Machine Cursor -> Machine -> Machine
swapCursors a b m = set b (m^.a) $ (set a (m^.b) m)

rotateCursorsDown :: Machine -> Machine
rotateCursorsDown m =
    fetchCursor p topCursor .
    swapCursors midCursor topCursor .
    swapCursors botCursor midCursor .
    writeCursor botCursor $ m
    where
        p = case m^.topCursor.cursorSentence.priWord of
            (M p) -> p
            NullWord -> emptyCursor^.cursorPointer
            _ -> error "Primary word in top cursor must be M to rotate down!"

rotateCursorsUp :: Machine -> Machine
rotateCursorsUp m =
    fetchCursor p botCursor .
    swapCursors midCursor botCursor .
    swapCursors topCursor midCursor .
    writeCursor topCursor $ m
    where
        p = case m^.botCursor.cursorSentence.priWord of
            (N p) -> p
            _ -> error "Primary word in bottom cursor must be N to rotate up!"

-- Primitive Predicates
noWord :: Lens' Machine Cursor -> Machine -> Bool
noWord c m = all (flip view m) [c.word0.to isNull, c.word1.to isNull, c.word2.to isNull]

oneWord :: Lens' Machine Cursor -> Machine -> Bool
oneWord c m = all (flip view m) [c.word0.to isNotNull, c.word1.to isNull, c.word2.to isNull]

twoWord :: Lens' Machine Cursor -> Machine -> Bool
twoWord c m = all (flip view m) [c.word0.to isNotNull, c.word1.to isNotNull, c.word2.to isNull]

threeWord :: Lens' Machine Cursor -> Machine -> Bool
threeWord c m = all (flip view m) [c.word0.to isNotNull, c.word1.to isNotNull, c.word2.to isNotNull]

oneWord' :: Lens' Machine Cursor -> Machine -> Bool
oneWord' c m = all (flip view m) [c.word0.to isNotNull]

twoWord' :: Lens' Machine Cursor -> Machine -> Bool
twoWord' c m = all (flip view m) [c.word0.to isNotNull, c.word1.to isNotNull]

-- Cursor/Word Lens shorthands
word0 :: Lens' Cursor Word
word0 = cursorSentence.priWord

word1 :: Lens' Cursor Word
word1 = cursorSentence.secWord

word2 :: Lens' Cursor Word
word2 = cursorSentence.triWord

c0w0 :: Lens' Machine Word
c0w0 = botCursor.word0
c0w1 :: Lens' Machine Word
c0w1 = botCursor.word1
c0w2 :: Lens' Machine Word
c0w2 = botCursor.word2

c1w0 :: Lens' Machine Word
c1w0 = midCursor.word0
c1w1 :: Lens' Machine Word
c1w1 = midCursor.word1
c1w2 :: Lens' Machine Word
c1w2 = midCursor.word2

c2w0 :: Lens' Machine Word
c2w0 = topCursor.word0
c2w1 :: Lens' Machine Word
c2w1 = topCursor.word1
c2w2 :: Lens' Machine Word
c2w2 = topCursor.word2
