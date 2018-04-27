module Combinatron.Sparker (
  sparkRandom
) where

import Prelude hiding (Word)
import Combinatron.Predicates
import Combinatron.Operations
import Combinatron.Types hiding (isP, isG, p, g)
import Control.Lens (view)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)
import qualified Data.HashMap.Strict as M

-- | Spark a task starting at a random undead sentence in the index without including
-- the first sentence
sparkRandom :: Machine -> Machine
sparkRandom m = sparkTask (referencedPointers !! rIndex) m
    where
        -- this is considering a zero indexed array, which will be converted to 1-indexed.
        -- Start at the second index, and end at the last.
        rIndex = unsafePerformIO $ randomRIO (0, (pred $ length referencedPointers))
        -- Sometimes a task can be sparked for a dead cell. It can be the case
        -- that this cell is moved to another dead cell such that it now has a
        -- self-reference, which is illegal. This can't happen with undead
        -- cells.
        referencedPointers = M.keys $ M.filter ((/=) Dead) (view garbageCollector m)

canSparkTask :: Pointer -> (Machine -> Machine) -> Machine -> Machine
canSparkTask p f m
    | isMWord p m = m -- can't spark a task on a partially evaluated expression
    | isRootsFull m = m
    | otherwise = f m

isMWord :: Pointer -> Machine -> Bool
isMWord p m = isM w
    where
        s = sentenceAt p (view sentenceIndex m)
        w = view priWord s

-- To spark a task, manipulate index to include sparked word
-- Add location of new, original sentence to task roots.
sparkTask :: Pointer -> Machine -> Machine
sparkTask index m = canSparkTask index (uncurry addRoot . sparkWord index) m

-- Move the sentence at the location
-- Write a single Sparked word in the index location, with the location of the moved sentence
sparkWord :: Pointer -> Machine -> (Pointer, Machine)
sparkWord index m = (p, m'')
    where
        s = sentenceAt index (view sentenceIndex m)
        (p, m') = addSentence s m
        m'' = setSentenceMachine index (Sentence (Sparked p) NullWord NullWord) m'
