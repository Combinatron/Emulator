{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}
module Combinatron.Types.Printer where

import Prelude hiding (Word)
import qualified Data.Vector as V
import Data.List (intersperse)
import Control.Lens
import Combinatron.Types.Memory
import Combinatron.Types.Instructions
import Combinatron.Types.Machine
import Combinatron.Types.Evaluator

-- | Printing facilities
class PrettyPrinter a where
    prettyPrint :: a -> String

instance PrettyPrinter Word where
    prettyPrint (N (Pointer p)) = "N" ++ show p
    prettyPrint (M (Pointer p)) = "M" ++ show p
    prettyPrint (G (Pointer p)) = "G" ++ show p
    prettyPrint (P (Pointer p)) = "P" ++ show p
    prettyPrint NullWord = "0"
    prettyPrint w = show w

instance PrettyPrinter Sentence where
    prettyPrint (Sentence w w' w'') = "<" ++ prettyPrint w ++ " " ++ prettyPrint w' ++ " " ++ prettyPrint w'' ++ ">"

instance PrettyPrinter Cursor where
    prettyPrint (Cursor (Pointer p) s) = "{" ++ show p ++ " " ++ prettyPrint s ++ "}"

instance PrettyPrinter SentenceIndex where
    prettyPrint index = foldr (++) "" . intersperse ", " . map prettyPrint $ (V.toList index)

instance PrettyPrinter Machine where
    prettyPrint machine = foldr (++) "" . intersperse "\n" $
        [ "Machine"
        , "Cursors:"
        , "- top: " ++ prettyPrint (machine^.topCursor)
        , "- mid: " ++ prettyPrint (machine^.midCursor)
        , "- bot: " ++ prettyPrint (machine^.botCursor)
        , "Index: " ++ prettyPrint (machine^.sentenceIndex)
        , "Value: " ++ prettyPrint (machine^.value)
        ]

printMachine :: Machine -> IO Machine
printMachine m = do
    putStrLn . prettyPrint $ m
    return m
