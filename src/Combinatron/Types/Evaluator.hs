{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}
module Combinatron.Types.Evaluator where

import qualified Data.Vector as V
import Control.Lens
import Data.Maybe (fromMaybe)
import Combinatron.Types.Memory

data Cursor = Cursor { _cursorPointer :: Pointer, _cursorSentence ::  Sentence }
    deriving (Show, Eq)

-- | Lenses
makeLenses ''Cursor

-- | An empty cursor is one that has a null pointer and all NullWords in its sentence.
emptyCursor :: Cursor
emptyCursor = Cursor nullPointer emptySentence

-- Helper to create a cursor from a given sentence
cursorAt :: Pointer -> SentenceIndex -> Cursor
cursorAt p index = fromMaybe emptyCursor (usePointer p Nothing (\x -> Cursor p <$> index V.!? x))
