module Combinatron.Types.GarbageCollector where

import Combinatron.Types.Memory
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

type Collector = HashMap Pointer Liveness
type References = [Pointer]

data Liveness = Dead | Referenced | Unreferenced
    deriving (Eq, Show)

initialize = M.empty
