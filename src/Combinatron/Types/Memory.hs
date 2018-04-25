{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell, GeneralizedNewtypeDeriving  #-}
module Combinatron.Types.Memory where

import Prelude hiding (Word)
import qualified Data.Vector as V
import Control.Lens
import Data.Maybe (fromMaybe)
import Combinatron.Types.Instructions (Word(NullWord))
import Combinatron.Types.Parameters (indexSize)
import Data.Hashable (Hashable)

-- | A Pointer is just a wrapper around an Int. It only exposes printing and equality functionality.
newtype Pointer = Pointer Int
    deriving (Show, Eq, Hashable, Ord)

nullPointer = Pointer 0

-- | Non-null pointers must be greater than 0, so only this constructor is exported from the module.
newPointer :: Int -> Pointer
newPointer p
    | p > 0 && p < indexSize = Pointer p
    | otherwise = error $ "Pointers must be greater than 0 and less than " ++ show indexSize ++ "!"

-- | Dereferencing a pointer can be a bit cumbersome, since 0 is a special value. This helper makes that process a bit easier.
usePointer :: Pointer -> a -> (Int -> a) -> a
usePointer (Pointer 0) x _ = x
usePointer (Pointer x) _ f = f (pred x)

data Sentence = Sentence
    { _priWord :: Word
    , _secWord :: Word
    , _triWord :: Word
    }
    deriving (Show, Eq)

type SentenceIndex = V.Vector Sentence

-- | Lenses
makeLenses ''Sentence

-- | This is a little helper for making it easier to construct sentences.
s :: (Word, Word, Word) -> Sentence
s (w, w', w'') = Sentence w w' w''

-- | This is a little helper for making it easier to construct programs from lists of Word tuples.
program :: [(Word, Word, Word)] -> V.Vector Sentence
program = V.fromList . map (s $)

emptySentence :: Sentence
emptySentence = Sentence NullWord NullWord NullWord

sentenceAt :: Pointer -> SentenceIndex -> Sentence
sentenceAt p index = fromMaybe emptySentence (usePointer p Nothing (\x -> index V.!? x))
