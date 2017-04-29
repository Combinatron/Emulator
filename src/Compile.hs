module Main where

import System.Environment (getArgs)
import Combinatron.Compiler
import Combinatron.Assembler
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
    (input:output:[]) <- getArgs
    s <- readFile input
    let compiled = compile s
        assembled = assembleFile (V.fromList compiled)
    B.writeFile output assembled
