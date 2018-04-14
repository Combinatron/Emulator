module Combinatron.Types (
    Word(..),
    Pointer(), newPointer, usePointer,
    Cursor(..),
    Sentence(..),
    SentenceIndex,
    Machine(..),
    Task(..), newTask, topPointer, midPointer, botPointer,
    prettyPrint,
    printMachine,
    n, m, g, p, s, sparked, program, emptyCursor, emptySentence, isN, isM, isP, isG, isSparked, isNotNull, isNull,
    initialize, cursorAt, sentenceAt,
    -- lenses
    topCursor, midCursor, botCursor, sentenceIndex, value,
    priWord, secWord, triWord,
    cursorPointer, cursorSentence,
    nodeRoots,
    -- Machine Manipulation
    setSentenceMachine
) where

import Prelude hiding (Word)
import Combinatron.Types.Printer
import Combinatron.Types.Machine
import Combinatron.Types.Memory
import Combinatron.Types.Instructions
import Combinatron.Types.Evaluator
