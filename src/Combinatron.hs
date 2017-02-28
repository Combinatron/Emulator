{-# LANGUAGE Rank2Types #-}
module Combinatron (
  run,
  runDebug
) where

import Prelude hiding (Word(..))
import Combinatron.Operations
import Combinatron.Types
import Control.Lens (Lens', view, to)
import System.IO.Unsafe (unsafePerformIO)

run m = case step m of
    (Right m) -> run m
    (Left m) -> m

runDebug m = case step (unsafePerformIO (printMachine m)) of
    (Right m) -> run m
    (Left m) -> m

step :: Machine -> Either Machine Machine
step m
    | isNest m = Right $ nest m
    | isUnnest m = Right $ unnest m
    | isK1 m = Right $ k1 m
    | isK2 m = Right $ k2 m
    | isW1 m = Right $ w1 m
    | isW2 m = Right $ w2 m
    | isC1 m = Right $ c1 m
    | isC2 m = Right $ c2 m
    | isC3 m = Right $ c3 m
    | isB1 m = Right $ b1 m
    | isB2 m = Right $ b2 m
    | isB3 m = Right $ b3 m
    | otherwise = Left m

-- | Predicates
isNest :: Machine -> Bool
isNest = view (c0w0.to isN)

isUnnest :: Machine -> Bool
isUnnest m = all ($ m) [oneWord botCursor, oneWord' midCursor]

isK1 :: Machine -> Bool
isK1 m = all ($ m) [primaryWord K, threeWord botCursor]

isK2 :: Machine -> Bool
isK2 m = all ($ m) [primaryWord K, twoWord botCursor, twoWord' midCursor]

isW1 :: Machine -> Bool
isW1 m = all ($ m) [primaryWord W, threeWord botCursor]

isW2 :: Machine -> Bool
isW2 m = all ($ m) [primaryWord W, twoWord botCursor, twoWord' midCursor]

isC1 :: Machine -> Bool
isC1 m = all ($ m) [primaryWord C, threeWord botCursor, twoWord' midCursor]

isC2 :: Machine -> Bool
isC2 m = all ($ m) [primaryWord C, twoWord botCursor, threeWord midCursor]

isC3 :: Machine -> Bool
isC3 m = all ($ m) [primaryWord C, twoWord botCursor, twoWord midCursor, twoWord' topCursor]

isB1 :: Machine -> Bool
isB1 m = all ($ m) [primaryWord B, threeWord botCursor, twoWord' midCursor]

isB2 :: Machine -> Bool
isB2 m = all ($ m) [primaryWord B, twoWord botCursor, threeWord midCursor]

isB3 :: Machine -> Bool
isB3 m = all ($ m) [primaryWord B, twoWord botCursor, twoWord midCursor, twoWord' topCursor]

primaryWord :: Word -> Machine -> Bool
primaryWord w m = view c0w0 m == w

-- Nesting/Unnesting
nest :: Machine -> Machine
nest m = newMWord . rotateCursorsUp $ m

unnest :: Machine -> Machine
unnest m = rotateCursorsDown . copyWord c0w0 c1w0 $ m

-- Reductions
k1 :: Machine -> Machine
k1 m = zeroWord c0w1 . zeroWord c0w2 . swapWords c0w0 c0w1 $ m

k2 :: Machine -> Machine
k2 m = rotateCursorsDown . zeroWord c1w2 . copyWord c1w2 c1w1 . copyWord c0w1 c1w0 $ m

w1 :: Machine -> Machine
w1 m = copyWord c0w2 c0w1 . copyWord c0w1 c0w0 $ m

w2 :: Machine -> Machine
w2 m = rotateCursorsDown . addSentence (s (arg1, arg2, NullWord)) c1w0 $ m
    where
        arg1 = view c0w1 m
        arg2 = view c1w1 m

c1 :: Machine -> Machine
c1 m = rotateCursorsDown . addSentence (s (arg1, arg2, NullWord)) c1w0 . copyWord c0w2 c1w1 $ m
    where
        arg1 = view c0w1 m
        arg2 = view c1w1 m

c2 :: Machine -> Machine
c2 m = rotateCursorsDown . swapWords c1w1 c1w2 . copyWord c0w1 c1w0 $ m

c3 :: Machine -> Machine
c3 m = rotateCursorsDown . step2 . rotateCursorsDown . step1 $ m
    where
        step1 = newNWord . addSentence (s (view c0w1 m, view c2w1 m, NullWord)) c2w1
        step2 = copyWord c0w1 c1w1 . copyWord c1w1 c1w0

b1 :: Machine -> Machine
b1 m = rotateCursorsDown . copyWord c0w1 c1w0 . addSentence (s (arg1, arg2, NullWord)) c1w1 $ m
    where
        arg1 = view c0w2 m
        arg2 = view c1w1 m

b2 :: Machine -> Machine
b2 m = rotateCursorsDown . zeroWord c1w2 . copyWord c0w1 c1w0 . addSentence (s (arg1, arg2, NullWord)) c1w1 $ m
    where
        arg1 = view c1w1 m
        arg2 = view c1w2 m

b3 :: Machine -> Machine
b3 m = rotateCursorsDown . step2 . rotateCursorsDown . step1 $ m
    where
        step1 = newNWord . copyWord c0w1 c2w1 . addSentence (s (view c1w1 m, view c2w1 m, NullWord)) c1w2
        step2 = zeroWord c0w2 . swapWords c0w2 c1w1 . swapWords c1w1 c1w0
