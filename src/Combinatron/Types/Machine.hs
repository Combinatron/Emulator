{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}
module Combinatron.Types.Machine where

import Control.Lens
import Combinatron.Types.Memory
import Combinatron.Types.Evaluator

data Machine = Machine
    { _topCursor :: Cursor
    , _midCursor :: Cursor
    , _botCursor :: Cursor
    , _sentenceIndex :: SentenceIndex
    , _value :: Sentence
    }
    deriving (Show)

-- | Lenses
makeLenses ''Machine

-- | Builds a machine from a given SentenceIndex. The SentenceIndex is essentially just a program. By convention, the first sentence in the index is the starting point.
initialize :: SentenceIndex -> Machine
initialize index = Machine
    { _topCursor = emptyCursor
    , _midCursor = emptyCursor
    , _botCursor = cursorAt (newPointer 1) index
    , _sentenceIndex = index
    , _value = emptySentence
    }
