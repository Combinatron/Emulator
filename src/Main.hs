module Main where

import Combinatron (run)
import Combinatron.Programs (w1Prog)
import Combinatron.Types (initialize, printMachine)

main = printMachine $ run (initialize w1Prog)
