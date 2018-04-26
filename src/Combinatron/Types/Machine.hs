{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}
module Combinatron.Types.Machine where

import Control.Lens
import Combinatron.Types.Memory
import Combinatron.Types.Evaluator
import Combinatron.Types.GarbageCollector
import qualified Data.Vector as V

data Machine = Machine
    { _topCursor :: Cursor
    , _midCursor :: Cursor
    , _botCursor :: Cursor
    , _sentenceIndex :: SentenceIndex
    , _nodeRoots :: TaskQueue
    , _value :: Sentence
    , _garbageCollector :: Collector
    }
    deriving (Show)

data Task = Task
    { _botPointer :: Pointer
    , _midPointer :: Pointer
    , _topPointer :: Pointer
    , _taskId :: Int
    }
    deriving (Show)

data TaskQueue = TaskQueue
    { _taskQueue :: V.Vector Task
    , _nextTaskId :: Int
    }
    deriving (Show)

-- | Lenses
makeLenses ''Machine
makeLenses ''Task
makeLenses ''TaskQueue

newTask :: Pointer -> TaskQueue -> TaskQueue
newTask p tq =  withTask . withNewId $ tq
    where
        withTask = over taskQueue (\ q -> q `V.snoc` (Task p nullPointer nullPointer (tq^.nextTaskId)))
        withNewId = over nextTaskId succ

emptyTaskQueue :: TaskQueue
emptyTaskQueue = TaskQueue V.empty 1

-- | Builds a machine from a given SentenceIndex. The SentenceIndex is essentially just a program. By convention, the first sentence in the index is the starting point.
initialize :: SentenceIndex -> Machine
initialize index = Machine
    { _topCursor = emptyCursor
    , _midCursor = emptyCursor
    , _botCursor = cursorAt (newPointer 1) index
    , _sentenceIndex = index
    , _nodeRoots = newTask (newPointer 1) emptyTaskQueue
    , _value = emptySentence
    , _garbageCollector = Combinatron.Types.GarbageCollector.initialize
    }

setSentenceMachine :: Pointer -> Sentence -> Machine -> Machine
setSentenceMachine p s m = m & sentenceIndex %~ (setSentence p s)

setSentence :: Pointer -> Sentence -> SentenceIndex -> SentenceIndex
setSentence p s si = usePointer p si (\i -> si V.// [(i, s)])
