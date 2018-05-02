module Combinatron (
  run,
  runDebug,
  runN,
  cycle,
  megacycle
) where

import Prelude hiding (cycle)
import Combinatron.Types (printMachine, Machine)
import Combinatron.Reducer
import Combinatron.Sparker
import Combinatron.Operations (updateRoot)
import Combinatron.GarbageCollector

type MachineExecution = ExecutionStep Machine Machine

infinity = 1/0

run :: MachineExecution -> MachineExecution
run = runN megacycle infinity

runDebug :: MachineExecution -> IO MachineExecution
runDebug m = do
    prompt
    let m' = runN (fmap collect . cycle) 1 m
    printMachine (unwrapExecutionStep m')
    putStrLn "Sparking task..."
    prompt
    let m'' = runN (fmap (collect . sparkRandom) . fmap collect . cycle) 1 m'
    printMachine (unwrapExecutionStep m'')
    runDebug m''

prompt = getLine

megacycle :: MachineExecution -> MachineExecution
megacycle m = fmap (collect . sparkRandom) . cycle . fmap collect . cycle $ m

runN ::
    (Num a, Eq a, Enum a) =>
    (MachineExecution -> MachineExecution) ->
    a -> MachineExecution -> MachineExecution
runN _ 0 m = m
runN f n m =
    case r of
        (Stopped m) -> Stopped m
        _ -> runN f (pred n) r
    where
        r = f m

cycle :: MachineExecution -> MachineExecution
cycle = step . updateRoot . unwrapExecutionStep
