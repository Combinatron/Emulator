module Combinatron (
  run,
  runDebug,
  runN,
  runner
) where

import Combinatron.Types (printMachine, Machine)
import Combinatron.Reducer
import Combinatron.Sparker
import Combinatron.Operations (updateRoot)
import Combinatron.GarbageCollector
import Control.Monad ((=<<))

infinity = 1/0

run = runN infinity

runDebug m = do
    prompt
    let m' = runN 1 m
    printMachine m'
    runDebug m'

prompt = getLine

runner m r = fmap sparker $ stepTwo =<< stepOne
    where
        stepOne = fmap collect (r m)
        stepTwo m' = fmap collect (r m')
        sparker m'' = sparkRandom m''

runN 0 m = m
runN n m = case runner m r of
    (Right m') -> runN (n - 1) m'
    (Left m) -> m
    where
        r :: Machine -> Either Machine Machine
        r = step . updateRoot
