module Combinatron.Types (
    Word(..),
    Pointer(), newPointer, usePointer,
    Cursor(..),
    Sentence(..),
    SentenceIndex,
    Machine(..),
    prettyPrint,
    printMachine,
    n, m, g, p, s, program, emptyCursor, emptySentence, isN, isM, isP, isG, isNotNull, isNull,
    initialize, cursorAt, sentenceAt,
    -- lenses
    topCursor, midCursor, botCursor, sentenceIndex, value,
    priWord, secWord, triWord,
    cursorPointer, cursorSentence
) where

import Prelude hiding (Word)
import Combinatron.Types.Internal
