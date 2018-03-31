module Combinatron (
  run,
  runDebug,
  runN
) where

import Combinatron.Types (printMachine)
import System.IO.Unsafe (unsafePerformIO)
import Combinatron.Reducer

run m = case step m of
    (Right m) -> run m
    (Left m) -> m

runDebug m = case step (unsafePerformIO (printMachine m)) of
    (Right m) -> runDebug m
    (Left m) -> m

runN 0 m = m
runN n m = case step m of
    (Right m) -> runN (n - 1) m
    (Left m) -> m
