module Main where

import Combinatron (run)
import System.Environment (getArgs)
import Combinatron.Types (initialize, printMachine)
import Combinatron.Loader
import qualified Data.ByteString.Lazy as B

main = do
    input <- head <$> getArgs
    prog <- loadFile <$> B.readFile input
    printMachine $ run (initialize prog)
