{-# LANGUAGE TemplateHaskell #-}
module Combinatron.Types.Statistics where

import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import Data.Time.Clock
import Control.Lens

data Statistics = Statistics
    { _cycleCount :: Integer
    , _taskSwitchCount :: Integer
    , _taskStartTimes :: HashMap Int UTCTime
    , _taskEndTimes :: HashMap Int UTCTime
    , _taskCompletionTimes :: HashMap Int NominalDiffTime
    }
    deriving (Show)

makeLenses ''Statistics

initialize = Statistics 0 0 H.empty H.empty H.empty

countTaskSwitch :: Statistics -> Statistics
countTaskSwitch s = s & taskSwitchCount %~ succ

countCycle :: Statistics -> Statistics
countCycle s = s & cycleCount %~ succ

startTask :: Int -> Statistics -> IO Statistics
startTask taskId stats = do
    t <- getCurrentTime
    return $ stats & taskStartTimes %~ (H.insert taskId t)

finishTask :: Int -> Statistics -> IO Statistics
finishTask taskId stats = do
    t <- getCurrentTime
    let stats' = stats & taskEndTimes %~ (H.insert taskId t)
        start = H.lookupDefault t taskId (stats'^.taskStartTimes)
    return $ stats' & taskCompletionTimes %~ (H.insert taskId (diffUTCTime t start))
