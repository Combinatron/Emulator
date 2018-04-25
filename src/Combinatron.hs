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

runDebug c m = do
    prompt
    let (c', m') = runN 1 c m
    printMachine m
    printCollector c'
    runDebug c' m'

prompt = getLine

runner c m r = fmap sparker $ stepTwo =<< stepOne
    where
        stepOne = fmap (collect c) (r m)
        stepTwo (c', m') = fmap (collect c') (r m')
        sparker (c'', m'') = (c'', sparkRandom m'')

runN 0 c m = (c, m)
runN n c m = case runner c m r of
    (Right (c', m')) -> runN (n - 1) c' m'
    (Left m) -> (c, m)
    where
        r :: Machine -> Either Machine Machine
        r = step . updateRoot
