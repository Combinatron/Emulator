module Main where

import System.Environment (getArgs)
import Combinatron.Compiler

main :: IO ()
main = do
    arg <- head <$> getArgs
    s <- readFile arg
    let compiled = compile s
    print compiled
