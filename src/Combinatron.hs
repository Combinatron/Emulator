module Combinatron (
  run,
  runDebug,
  runN,
  cycle
) where

import Prelude hiding (cycle)
import Combinatron.Types (printMachine, Machine)
import Combinatron.Reducer
import Combinatron.Sparker
import Combinatron.Operations (updateRoot)
import Combinatron.GarbageCollector
import Control.Monad ((=<<))

infinity = 1/0

run = runN megacycle infinity

runDebug m = do
    prompt
    let m' = runN cycle 1 m
    printMachine m'
    prompt
    let m'' = runN (fmap sparkRandom . cycle) 1 m'
    printMachine m''
    runDebug m''

prompt = getLine

megacycle m = fmap sparkRandom $ cycle =<< cycle m

runN :: (Num a, Eq a, Enum a) => (Machine -> Either Machine Machine) -> a -> Machine -> Machine
runN _ 0 m = m
runN f n m = case f m of
    (Right m') -> runN f (pred n) m'
    (Left m) -> m

cycle :: Machine -> Either Machine Machine
cycle = fmap collect . step . updateRoot
