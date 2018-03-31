module Combinatron.Sparker (
  sparkRandom
) where

import Prelude hiding (Word)
import Combinatron.Predicates
import Combinatron.Operations
import Combinatron.Types hiding (isP, isG, p, g)
import Control.Lens (view, to)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector as V
import System.Random (randomRIO)

-- | Spark a task starting at a random sentence in the index without including
-- the first sentence
sparkRandom :: Machine -> Machine
sparkRandom m
    | isRootsFull m = m
    | otherwise = addRoot (newPointer rIndex) m
    where
        rIndex = unsafePerformIO $ randomRIO (2, view (sentenceIndex.to V.length) m)
