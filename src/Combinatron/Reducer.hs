{-# LANGUAGE Rank2Types #-}
module Combinatron.Reducer where

import Prelude hiding (Word)
import Combinatron.Operations
import Combinatron.Predicates
import Combinatron.Types hiding (isP, isG, isSparked, p, g, sparked)
import Control.Lens (view)

step :: Machine -> Either Machine Machine
step m
    | isLoneNest m = Right $ loneNest m
    | isNest m = Right $ nest m
    | isSparked m = Right $ sparked m -- Must come before unnest check
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
    | isP m = Right $ p m
    | isG m = Right $ g m
    | isY m = Right $ y m
    | isI m = Right $ i m
    | not (isRootsEmpty m) = Right $ whnf m
    | isInvalidUnnest m = Right $ deadTask m
    | otherwise = Left m

-- Cursor rotation
rotateCursorsDown :: Machine -> Machine
rotateCursorsDown m =
    fetchCursor p topCursor .
    swapCursors midCursor topCursor .
    swapCursors botCursor midCursor .
    writeCursor botCursor $ m
    where
        p = case view (topCursor.cursorSentence.priWord) m of
            (M p) -> p
            NullWord -> view cursorPointer emptyCursor
            _ -> error "Primary word in top cursor must be M to rotate down!"

rotateCursorsUp :: Machine -> Machine
rotateCursorsUp m =
    fetchCursor p botCursor .
    swapCursors midCursor botCursor .
    swapCursors topCursor midCursor .
    writeCursor topCursor $ m
    where
        p = case view (botCursor.cursorSentence.priWord) m of
            (N p) -> p
            _ -> error "Primary word in bottom cursor must be N to rotate up!"

-- Nesting/Unnesting
nest :: Machine -> Machine
nest m = newMWord . rotateCursorsUp $ m

loneNest :: Machine -> Machine
loneNest m = case (view c0w0 m) of
    (N p) -> fetchCursor p botCursor $ m
    _ -> error "loneNest must be called on an N word!"

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
w2 m = rotateCursorsDown . addSentenceAndUpdate (s (arg1, arg2, NullWord)) c1w0 $ m
    where
        arg1 = view c0w1 m
        arg2 = view c1w1 m

c1 :: Machine -> Machine
c1 m = rotateCursorsDown . addSentenceAndUpdate (s (arg1, arg2, NullWord)) c1w0 . copyWord c0w2 c1w1 $ m
    where
        arg1 = view c0w1 m
        arg2 = view c1w1 m

c2 :: Machine -> Machine
c2 m = rotateCursorsDown . swapWords c1w1 c1w2 . copyWord c0w1 c1w0 $ m

c3 :: Machine -> Machine
c3 m = rotateCursorsDown . step2 . rotateCursorsDown . step1 $ m
    where
        step1 = newNWord . addSentenceAndUpdate (s (view c0w1 m, view c2w1 m, NullWord)) c2w1
        step2 = copyWord c0w1 c1w1 . copyWord c1w1 c1w0

b1 :: Machine -> Machine
b1 m = rotateCursorsDown . copyWord c0w1 c1w0 . addSentenceAndUpdate (s (arg1, arg2, NullWord)) c1w1 $ m
    where
        arg1 = view c0w2 m
        arg2 = view c1w1 m

b2 :: Machine -> Machine
b2 m = rotateCursorsDown . zeroWord c1w2 . copyWord c0w1 c1w0 . addSentenceAndUpdate (s (arg1, arg2, NullWord)) c1w1 $ m
    where
        arg1 = view c1w1 m
        arg2 = view c1w2 m

b3 :: Machine -> Machine
b3 m = rotateCursorsDown . step2 . rotateCursorsDown . step1 $ m
    where
        step1 = newNWord . copyWord c0w1 c2w1 . addSentenceAndUpdate (s (view c1w1 m, view c2w1 m, NullWord)) c1w2
        step2 = zeroWord c0w2 . swapWords c0w2 c1w1 . swapWords c1w1 c1w0

g :: Machine -> Machine
g m = case view c0w0 m of
    (G p) -> i . getValue p $ m
    _ -> error "getValue must be called on a G word!"

p :: Machine -> Machine
p m = case view c0w0 m of
    (P p) -> i . putValue p $ m
    _ -> error "putValue must be called on a P word!"

y :: Machine -> Machine
y m = swapWords c0w0 c0w1 . swapWords c0w0 c1w0 . newNWord . swapWords c0w0 c1w0 $ m

i :: Machine -> Machine
i m = zeroWord c0w2 . swapWords c0w1 c0w2 . swapWords c0w0 c0w1 $ m

-- Don't task switch immediately, instead convert Sparked word to an N word
-- first and then task switch. This allows for forcing the task on the next
-- round.
sparked :: Machine -> Machine
sparked m = taskSwitch . addSentenceAndUpdate (s (N p, NullWord, NullWord)) c0w0 $ m
    where
        (Sparked p) = view c0w0 m

-- Write out all cursors to index
-- Read new root from task index
-- Load cursor from root into bottom cursor
taskSwitch :: Machine -> Machine
taskSwitch m = loadRoot . rotateRoots . zeroCursors . writeCursors $ m

whnf :: Machine -> Machine
whnf m = loadRoot . removeRoot . zeroCursors . writeCursors $ m

deadTask :: Machine -> Machine
deadTask = whnf
