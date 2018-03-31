module Combinatron.Types (
    Word(..),
    Pointer(), newPointer, usePointer,
    Cursor(..),
    Sentence(..),
    SentenceIndex,
    Machine(..), nodeRootSize,
    prettyPrint,
    printMachine,
    n, m, g, p, s, program, emptyCursor, emptySentence, isN, isM, isP, isG, isNotNull, isNull,
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
