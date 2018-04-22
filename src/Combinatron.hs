module Combinatron (
  run,
  runDebug,
  runN
) where

import Combinatron.Types (printMachine)
import System.IO.Unsafe (unsafePerformIO)
import Combinatron.Reducer
import Combinatron.Sparker
import Combinatron.Operations (updateRoot)

run m = case (sparkRandom . r  . r $ m) of
    (Right m) -> run m
    (Left m) -> m
    where
        r = step . updateRoot

runDebug m = case (sparkRandom . r . r $ m) of
    (Right m) -> runDebug m
    (Left m) -> m
    where
        r = step . updateRoot . unsafePerformIO . printMachine

runN 0 m = m
runN n m = case step m of
    (Right m) -> runN (n - 1) m
    (Left m) -> m
