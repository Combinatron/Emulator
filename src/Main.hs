module Main where

import Combinatron (loop)
import Combinatron.Programs (w1Prog)
import Combinatron.Types (initialize, printMachine)

main = printMachine $ loop (initialize w1Prog)
