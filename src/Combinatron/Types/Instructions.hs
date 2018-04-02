{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}
module Combinatron.Types.Instructions where

import Prelude hiding (Word)
import {-# SOURCE #-} Combinatron.Types.Memory (Pointer, newPointer)
import Combinatron.Types.Parameters (indexPower, nodeRootSize)

data Word = B | C | K | W | I | Y | N Pointer | M Pointer | G Pointer | P Pointer | NullWord | Sparked Pointer
    deriving (Show, Eq)

-- | This is a little helper for making it easier to construct N words.
n :: Int -> Word
n = N . newPointer

-- | This is a little helper for making it easier to construct M words.
m :: Int -> Word
m = M . newPointer

-- | This is a little helper for making it easier to construct G words.
g :: Int -> Word
g = G . newPointer

-- | This is a little helper for making it easier to construct P words.
p :: Int -> Word
p = P . newPointer

sparked :: Int -> Word
sparked = Sparked . newPointer

-- | Some comparison helpers
isN (N _) = True
isN _ = False

isM (M _) = True
isM _ = False

isG (G _) = True
isG _ = False

isP (P _) = True
isP _ = False

isSparked (Sparked _) = True
isSparked _ = False

isNotNull w = w /= NullWord
isNull w = w == NullWord

wordArgSize :: Word -> Int
wordArgSize (G _) = indexPower
wordArgSize (P _) = indexPower
wordArgSize (N _) = indexPower
wordArgSize (M _) = indexPower
wordArgSize (Sparked _) = ceiling . sqrt . fromIntegral $ nodeRootSize
wordArgSize _ = 0
