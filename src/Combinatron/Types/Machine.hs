{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}
module Combinatron.Types.Machine where

import Control.Lens
import Combinatron.Types.Memory
import Combinatron.Types.Evaluator
import qualified Data.Vector as V

data Machine = Machine
    { _topCursor :: Cursor
    , _midCursor :: Cursor
    , _botCursor :: Cursor
    , _sentenceIndex :: SentenceIndex
    , _nodeRoots :: TaskQueue
    , _value :: Sentence
    }
    deriving (Show)

type TaskQueue = V.Vector Pointer

-- | Lenses
makeLenses ''Machine

-- | Builds a machine from a given SentenceIndex. The SentenceIndex is essentially just a program. By convention, the first sentence in the index is the starting point.
initialize :: SentenceIndex -> Machine
initialize index = Machine
    { _topCursor = emptyCursor
    , _midCursor = emptyCursor
    , _botCursor = cursorAt (newPointer 1) index
    , _sentenceIndex = index
    , _nodeRoots = V.fromList [newPointer 1]
    , _value = emptySentence
    }

setSentenceMachine :: Pointer -> Sentence -> Machine -> Machine
setSentenceMachine p s m = m & sentenceIndex %~ (setSentence p s)

setSentence :: Pointer -> Sentence -> SentenceIndex -> SentenceIndex
setSentence p s si = usePointer p si (\i -> si V.// [(i, s)])
