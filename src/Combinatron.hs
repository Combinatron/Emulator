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
import Combinatron.GarbageCollector

run c m = case (runner c m r) of
    (Right (c, m)) -> run c m
    (Left m) -> m
    where
        r :: Machine -> Either Machine Machine
        r = step . updateRoot

runDebug c m = case (runner c m r) of
    (Right (c, m)) -> runDebug c m
    (Left  m) -> m
    where
        r :: Machine -> Either Machine Machine
        r = step . updateRoot . unsafePerformIO . (\m -> getLine >> printMachine m)

runner c m r = fmap sparker $ stepOne >>= stepTwo
    where
        stepOne = fmap (collect c) (r m)
        stepTwo (c', m') = fmap (collect c') (r m')
        sparker (c'', m'') = (c'', sparkRandom m'')

runN 0 m = m
runN n m = case step m of
    (Right m) -> runN (n - 1) m
    (Left m) -> m
