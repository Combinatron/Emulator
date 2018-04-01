module Combinatron.Types.Parameters where

-- Size of the task queue
nodeRootSize :: Int
nodeRootSize = 5

-- | Number of sentences in index
indexPower :: Int
indexPower = 12

indexSize :: Int
indexSize = 2^indexPower
