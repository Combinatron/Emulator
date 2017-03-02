{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}
module Combinatron.Types (
    Word(..),
    Pointer(), newPointer, usePointer,
    Cursor(..),
    Sentence(..),
    SentenceIndex,
    Machine(..),
    prettyPrint,
    printMachine,
    n, m, s, program, emptyCursor, isN, isM, isNotNull, isNull,
    initialize, cursorAt,
    -- lenses
    topCursor, midCursor, botCursor, sentenceIndex,
    priWord, secWord, triWord,
    cursorPointer, cursorSentence
) where

import Prelude hiding (Word)
import qualified Data.Vector as V
import Data.List (intersperse)
import Control.Lens
import Data.Maybe (fromMaybe)

-- | A Pointer is just a wrapper around an Int. It only exposes printing and equality functionality.
newtype Pointer = Pointer { pointer :: Int }
    deriving (Show, Eq)

-- | Pointers must be 0, so only this constructor is exported from the module.
newPointer :: Int -> Pointer
newPointer p
    | p > 0 = Pointer p
    | otherwise = error "Pointers must be greater than 0!"

-- | Dereferencing a pointer can be a bit cumbersome, since 0 is a special value. This helper makes that process a bit easier.
usePointer :: Pointer -> a -> (Int -> a) -> a
usePointer (Pointer 0) x _ = x
usePointer (Pointer x) _ f = f (pred x)

data Word = B | C | K | W | N Pointer | M Pointer | NullWord
    deriving (Show, Eq)

data Cursor = Cursor { _cursorPointer :: Pointer, _cursorSentence ::  Sentence }
    deriving (Show, Eq)

data Sentence = Sentence
    { _priWord :: Word
    , _secWord :: Word
    , _triWord :: Word
    }
    deriving (Show, Eq)

type SentenceIndex = V.Vector Sentence

data Machine = Machine
    { _topCursor :: Cursor
    , _midCursor :: Cursor
    , _botCursor :: Cursor
    , _sentenceIndex :: SentenceIndex
    }
    deriving (Show)

-- | Lenses
makeLenses ''Cursor
makeLenses ''Sentence
makeLenses ''Machine

-- | Printing facilities
class PrettyPrinter a where
    prettyPrint :: a -> String

instance PrettyPrinter Word where
    prettyPrint (N (Pointer p)) = "N" ++ show p
    prettyPrint (M (Pointer p)) = "M" ++ show p
    prettyPrint NullWord = "0"
    prettyPrint w = show w

instance PrettyPrinter Sentence where
    prettyPrint (Sentence w w' w'') = "<" ++ prettyPrint w ++ " " ++ prettyPrint w' ++ " " ++ prettyPrint w'' ++ ">"

instance PrettyPrinter Cursor where
    prettyPrint (Cursor (Pointer p) s) = "{" ++ show p ++ " " ++ prettyPrint s ++ "}"

instance PrettyPrinter SentenceIndex where
    prettyPrint index = foldr (++) "" . intersperse ", " . map prettyPrint $ (V.toList index)

instance PrettyPrinter Machine where
    prettyPrint machine = foldr (++) "" . intersperse "\n" $
        [ "Machine"
        , "Cursors:"
        , "- top: " ++ prettyPrint (machine^.topCursor)
        , "- mid: " ++ prettyPrint (machine^.midCursor)
        , "- bot: " ++ prettyPrint (machine^.botCursor)
        , "Index: " ++ prettyPrint (machine^.sentenceIndex)
        ]

printMachine :: Machine -> IO Machine
printMachine m = do
    putStrLn . prettyPrint $ m
    return m

-- | This is a little helper for making it easier to construct N words.
n :: Int -> Word
n = N . newPointer

-- | This is a little helper for making it easier to construct M words.
m :: Int -> Word
m = M . newPointer

-- | This is a little helper for making it easier to construct sentences.
s :: (Word, Word, Word) -> Sentence
s (w, w', w'') = Sentence w w' w''

-- | This is a little helper for making it easier to construct programs from lists of Word tuples.
program :: [(Word, Word, Word)] -> V.Vector Sentence
program = V.fromList . map (s $)

-- | An empty cursor is one that has a pointer to 0 and all NullWords in its sentence.
emptyCursor :: Cursor
emptyCursor = Cursor (Pointer 0) emptySentence

emptySentence :: Sentence
emptySentence = Sentence NullWord NullWord NullWord

-- | Some comparison helpers
isN (N _) = True
isN _ = False

isM (M _) = True
isM _ = False

isNotNull w = w /= NullWord
isNull w = w == NullWord

-- Helper to create a cursor from a given sentence
cursorAt :: Pointer -> SentenceIndex -> Cursor
cursorAt p index = fromMaybe emptyCursor (Cursor p <$> index V.!? pred (pointer p))

-- | Builds a machine from a given SentenceIndex. The SentenceIndex is essentially just a program. By convention, the first sentence in the index is the starting point.
initialize :: SentenceIndex -> Machine
initialize index = Machine
    { _topCursor = emptyCursor
    , _midCursor = emptyCursor
    , _botCursor = cursorAt (newPointer 1) index
    , _sentenceIndex = index
    }
