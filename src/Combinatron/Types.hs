{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}
module Combinatron.Types (
    Word(..),
    Pointer(..), newPointer, usePointer,
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

import Prelude hiding (Word(..))
import qualified Data.Vector as V
import Data.List (intersperse)
import Control.Lens.TH (makeLenses)
import Control.Lens
import Data.Maybe (fromMaybe)

newtype Pointer = Pointer { pointer :: Int }
    deriving (Show, Eq)

newPointer :: Int -> Pointer
newPointer p
    | p > 0 = Pointer p
    | otherwise = error "Pointers must be greater than 0!"

usePointer :: Pointer -> a -> (Int -> a) -> a
usePointer (Pointer 0) x _ = x
usePointer (Pointer x) _ f = f x

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

n :: Int -> Word
n = N . newPointer

m :: Int -> Word
m = M . newPointer

s :: (Word, Word, Word) -> Sentence
s (w, w', w'') = Sentence w w' w''

program :: [(Word, Word, Word)] -> V.Vector Sentence
program = V.fromList . map (s $)

emptyCursor :: Cursor
emptyCursor = Cursor (Pointer 0) (Sentence NullWord NullWord NullWord)

isN (N _) = True
isN _ = False

isM (M _) = True
isM _ = False

isNotNull w = w /= NullWord
isNull w = w == NullWord

-- Helper to create a cursor from a given sentence
cursorAt :: Pointer -> SentenceIndex -> Cursor
cursorAt p index = fromMaybe emptyCursor (Cursor p <$> index V.!? pred (pointer p))

initialize :: SentenceIndex -> Machine
initialize index = Machine
    -- By convention, the first sentence in the index is the starting point.
    { _topCursor = emptyCursor
    , _midCursor = emptyCursor
    , _botCursor = cursorAt (newPointer 1) index
    , _sentenceIndex = index
    }
