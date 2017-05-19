module Main where

import Combinatron (run)
import Combinatron.Types (initialize, printMachine)
import Combinatron.Loader
import qualified Data.ByteString.Lazy as B
import Options.Applicative.Simple (simpleOptions, addCommand, argument, str, metavar, pure)

commandLine =
    simpleOptions
        "prototype"
        "Combinatron Emulator"
        "An emulator for the Combinatron processor"
        (pure ()) $ do
            addCommand
                "run"
                "Run a program and print the final state"
                (\ s -> (run, s))
                (argument str (metavar "FILE"))

main = do
    (_, (runCmd, input)) <- commandLine
    prog <- loadFile <$> B.readFile input
    printMachine $ runCmd (initialize prog)
