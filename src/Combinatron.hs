module Combinatron (
  run,
  runDebug,
  runN
) where

import Combinatron.Types (printMachine, Machine)
import System.IO.Unsafe (unsafePerformIO)
import Combinatron.Reducer
import Combinatron.Sparker
import Combinatron.Operations (updateRoot)
import Control.Monad ((<=<))

run m = case (runner m) of
    (Right m) -> run m
    (Left m) -> m
    where
        r :: Machine -> Either Machine Machine
        r = step . updateRoot
        runner = fmap sparkRandom . r <=< r

runDebug m = case (runner m) of
    (Right m) -> runDebug m
    (Left m) -> m
    where
        r :: Machine -> Either Machine Machine
        r = step . updateRoot . unsafePerformIO . printMachine
        runner = fmap sparkRandom . r <=< r

runN 0 m = m
runN n m = case step m of
    (Right m) -> runN (n - 1) m
    (Left m) -> m
