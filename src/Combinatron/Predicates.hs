{-# LANGUAGE Rank2Types #-}
module Combinatron.Predicates where

import Prelude hiding (Word)
import Combinatron.Operations
import Combinatron.Types hiding (isP, isG, isSparked, p, g)
import Combinatron.Types.Parameters (nodeRootSize)
import qualified Combinatron.Types as Types
import Control.Lens (view, to)
import qualified Data.Vector as V

isRootsFull :: Machine -> Bool
isRootsFull = view (nodeRoots.to ((nodeRootSize ==) . V.length))

isRootsEmpty :: Machine -> Bool
isRootsEmpty = view (nodeRoots.to V.null)

isNest :: Machine -> Bool
isNest = view (c0w0.to isN)

isUnnest :: Machine -> Bool
isUnnest m = all ($ m) [oneWord botCursor, oneWord' midCursor]

-- Can never be a lone unnest operation
isLoneNest :: Machine -> Bool
isLoneNest m = isNest m && oneWord botCursor m

isG :: Machine -> Bool
isG = view (c0w0.to Types.isG)

isP :: Machine -> Bool
isP = view (c0w0.to Types.isP)

isY :: Machine -> Bool
isY = oneCursorSingleArg Y

isSparked :: Machine -> Bool
isSparked = view (c0w0.to Types.isSparked)

havingPropertiesAndWord :: [Machine -> Bool] -> Word -> Machine ->  Bool
havingPropertiesAndWord preds w m = all ($ m) ((primaryWord w):preds)

oneCursorSingleArg :: Word -> Machine -> Bool
oneCursorSingleArg w = havingPropertiesAndWord [twoWord' botCursor] w

oneCursor :: Word -> Machine -> Bool
oneCursor w = havingPropertiesAndWord [threeWord botCursor] w

twoCursorTwoArg :: Word -> Machine -> Bool
twoCursorTwoArg w = havingPropertiesAndWord [twoWord botCursor, twoWord' midCursor] w

twoCursorThreeArgBottom :: Word -> Machine -> Bool
twoCursorThreeArgBottom w = havingPropertiesAndWord [threeWord botCursor, twoWord' midCursor] w

twoCursorThreeArgMid :: Word -> Machine -> Bool
twoCursorThreeArgMid w = havingPropertiesAndWord [twoWord botCursor, threeWord midCursor] w

threeCursor :: Word -> Machine -> Bool
threeCursor w = havingPropertiesAndWord [twoWord botCursor, twoWord midCursor, twoWord' topCursor] w

isK1 :: Machine -> Bool
isK1 = oneCursor K

isK2 :: Machine -> Bool
isK2 = twoCursorTwoArg K

isW1 :: Machine -> Bool
isW1 = oneCursor W

isW2 :: Machine -> Bool
isW2 = twoCursorTwoArg W

isC1 :: Machine -> Bool
isC1 = twoCursorThreeArgBottom C

isC2 :: Machine -> Bool
isC2 = twoCursorThreeArgMid C

isC3 :: Machine -> Bool
isC3 = threeCursor C

isB1 :: Machine -> Bool
isB1 = twoCursorThreeArgBottom B

isB2 :: Machine -> Bool
isB2 = twoCursorThreeArgMid B

isB3 :: Machine -> Bool
isB3 = threeCursor B

isI :: Machine -> Bool
isI = oneCursorSingleArg I

primaryWord :: Word -> Machine -> Bool
primaryWord w m = view c0w0 m == w
