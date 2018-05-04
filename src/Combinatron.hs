module Combinatron (
  run,
  runDebug,
  runN,
  cycle,
  megacycle,
  instrument
) where

import Prelude hiding (cycle)
import Control.Lens
import Combinatron.Types
import Combinatron.Reducer
import Combinatron.Sparker
import Combinatron.Operations (updateRoot)
import Combinatron.GarbageCollector
import Combinatron.Types.Statistics
import qualified Data.HashMap.Strict as H
import Data.List ((\\))
import Safe (headMay)

type MachineExecution = ExecutionStep Machine Machine

printExecutionStep (Stopped _) = putStrLn "Stopped..."
printExecutionStep (TaskSwitch _) = putStrLn "TaskSwitch..."
printExecutionStep (TaskFinish _) = putStrLn "TaskFinish..."
printExecutionStep (TaskSpark _) = putStrLn "TaskSpark..."
printExecutionStep (Reduction _) = putStrLn "Reduction..."
printExecutionStep (Initialized _) = putStrLn "Initialized..."

run :: MachineExecution -> IO MachineExecution
run m = do
    m' <- instrument $ runN megacycle 1 m
    if finishedInitialTask (unwrapExecutionStep m')
    then return m'
    else run m'

finishedInitialTask :: Machine -> Bool
finishedInitialTask m = 1 `elem` finishedTaskIds m

startedTaskIds m = H.keys (m^.statistics.taskStartTimes)
finishedTaskIds m = H.keys (m^.statistics.taskEndTimes)
currentTaskIds m = map (view taskId) $ m^.nodeRoots.taskQueue ^.. folded

finishedTask m = headMay ((startedTaskIds m \\ finishedTaskIds m) \\ currentTaskIds m)
addedTask m = headMay (currentTaskIds m \\ startedTaskIds m)

-- TaskSwitch/TaskFinish don't seem to be working correctly
instrument :: MachineExecution -> IO MachineExecution
instrument m =
    case m of
        (Stopped m) -> return (Stopped m)
        (Initialized m) -> do
            newStats <- startTask 1 (m^.statistics)
            return $ Initialized (m & statistics .~ newStats)
        (TaskSwitch m) ->
            let newStats = countTaskSwitch (m^.statistics)
            in return $ TaskSwitch (m & statistics .~ newStats)
        (TaskFinish m) ->
            case finishedTask m of
                (Just i) -> do
                    newStats <- finishTask i (m^.statistics)
                    let m' = m & statistics .~ newStats
                    -- A task finish is always accompanied by the starting of a new task
                    (TaskSpark m'') <- instrument (TaskSpark m')
                    return $ TaskFinish m''
                Nothing -> return $ TaskFinish m
        (TaskSpark m) ->
            case addedTask m of
                (Just i) -> do
                    newStats <- startTask i (m^.statistics)
                    return $ TaskSpark (m & statistics .~ newStats)
                Nothing -> return $ TaskSpark m
        (Reduction m) ->
            let newStats = countCycle (m^.statistics)
            in return $ Reduction (m & statistics .~ newStats)

runDebug :: MachineExecution -> IO MachineExecution
runDebug m = do
    prompt
    m' <- instrument $ runN (fmap collect . cycle) 1 m
    printExecutionStep m'
    printMachine (unwrapExecutionStep m')
    putStrLn (prettyPrint ((unwrapExecutionStep m')^.statistics))
    if finishedInitialTask (unwrapExecutionStep m')
    then return m'
    else do
        putStrLn "Sparking task..."
        prompt
        m'' <- instrument $ runN (fmap (collect . sparkRandom) . fmap collect . cycle) 1 m'
        printExecutionStep m''
        printMachine (unwrapExecutionStep m'')
        putStrLn (prettyPrint ((unwrapExecutionStep m'')^.statistics))
        if finishedInitialTask (unwrapExecutionStep m'')
        then return m''
        else runDebug m''

prompt = return ()


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
