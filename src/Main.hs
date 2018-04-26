module Main where

import Combinatron (run, runDebug)
import Combinatron.Types (initialize, printMachine)
import Combinatron.Loader
import qualified Data.ByteString.Lazy as B
import Options.Applicative.Simple
import System.Random

data Options = Options
    { seed :: Maybe Int
    , file :: String
    }

parseOptions :: Parser Options
parseOptions = Options
    <$> optional (option auto
        (long "seed" <>
         short 's' <>
         metavar "INT" <>
         help "Seed for the random number generator"))
    <*> argument str (metavar "FILE")

commandLine =
    simpleOptions
        "prototype"
        "Combinatron Emulator"
        "An emulator for the Combinatron processor"
        (pure ()) $ do
            addCommand
                "run"
                "Run a program and print the final state"
                (\ o -> (\ y -> return (run y), o))
                parseOptions
            addCommand
                "debug"
                "Run a program and print each consecutive state"
                (\ o -> (runDebug, o))
                parseOptions

main = do
    (_, (runCmd, o)) <- commandLine
    prog <- loadFile <$> B.readFile (file o)
    putStrLn "Running..."
    let m = initialize prog
    rng <- case (seed o) of
        -- TODO: The rng has two "seeds", and we can only input one
        (Just i) -> return (mkStdGen i)
        Nothing -> fmap mkStdGen randomIO
    setStdGen rng
    putStrLn $ "Using random seed: " ++ show rng
    putStrLn $ "Subtract 1 when inputting seed"
    printMachine m
    m' <- runCmd m
    printMachine m'
