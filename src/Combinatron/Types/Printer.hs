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

instance PrettyPrinter (Int, Sentence) where
    prettyPrint (i, s) = show (succ i) ++ " " ++ prettyPrint s

instance PrettyPrinter Cursor where
    prettyPrint (Cursor (Pointer p) s) = "{" ++ show p ++ " " ++ prettyPrint s ++ "}"

instance PrettyPrinter (V.Vector Task) where
    prettyPrint = foldr (++) "" . intersperse "\n\t" . V.toList . V.imap (\ i m -> prettyPrint (i, m))

instance PrettyPrinter (V.Vector Sentence) where
    prettyPrint = foldr (++) "" . intersperse "\n\t" . V.toList . V.imap (\ i m -> prettyPrint (i, m))

instance PrettyPrinter TaskQueue where
    prettyPrint queue = concat
        [ "\n"
        , "- queue:\n\t", prettyPrint (queue^.taskQueue)
        , "\n"
        , "- id: ", show (queue^.nextTaskId)
        ]

instance PrettyPrinter Pointer where
    prettyPrint (Pointer p) = "Pointer " ++ show p

instance PrettyPrinter Machine where
    prettyPrint machine = foldr (++) "" . intersperse "\n" $
        [ "Machine"
        , "Cursors:"
        , "- top: " ++ prettyPrint (machine^.topCursor)
        , "- mid: " ++ prettyPrint (machine^.midCursor)
        , "- bot: " ++ prettyPrint (machine^.botCursor)
        , "Index:\n\t" ++ prettyPrint (machine^.sentenceIndex)
        , "Task Queue: " ++ prettyPrint (machine^.nodeRoots)
        , "Combinators: " ++ printCombinators machine
        , "Value: " ++ prettyPrint (machine^.value)
        ]

instance PrettyPrinter Task where
    prettyPrint task = concat
        [ "[ top "
        , prettyPrint (task^.topPointer)
        , ", mid "
        , prettyPrint (task^.midPointer)
        , ", bot "
        , prettyPrint (task^.botPointer)
        , ", id "
        , show (task^.taskId)
        , " ]"
        ]

instance PrettyPrinter (Int, Task) where
    prettyPrint (i, s) = show i ++ " " ++ prettyPrint s

-- Assumes 0 is always the root
-- No sharing of sentences
printCombinators :: Machine -> String
printCombinators m = lw
    where
        pc = m^.botCursor.cursorPointer
        pc' = m^.midCursor.cursorPointer
        rw = "#" ++ usePointer pc "null pointer" (\ x -> rightward x absorbed)
        lw = usePointer pc' rw (\ x -> leftward x rw absorbed)
        absorbed = absorbCursors (m^.sentenceIndex) [m^.topCursor, m^.midCursor, m^.botCursor]

absorbCursors :: SentenceIndex -> [Cursor] -> SentenceIndex
absorbCursors = foldl (\ si c -> setSentence (c^.cursorPointer) (c^.cursorSentence) si)

leftward :: Int -> String -> SentenceIndex -> String
leftward i s si =
    case si V.! i of
        (Sentence (M p) w2 w3) -> ascender p ("(" ++ s ++ ") " ++ words [w2, w3])
        _ -> error "Can not go leftwards without M word"
    where
        doPrint NullWord = ""
        doPrint (N p) = "(" ++ descender p si ++ ")"
        doPrint (M _) = error "Should not have M word in non-primary position"
        doPrint w = prettyPrint w
        words = foldl (++) "" . intersperse " " . filter (not . null) . map doPrint
        ascender p s = usePointer p s (\ x -> leftward x s si)

rightward :: Int -> SentenceIndex -> String
rightward i si =
    case si V.! i of
        (Sentence (M _) _ _) -> error "Can not go rightward into M word"
        (Sentence w1 w2 w3) -> words $ [w1, w2, w3]
    where
        doPrint NullWord = ""
        doPrint (N p) = "(" ++ descender p si ++ ")"
        doPrint (M _) = error "Should not have M word in non-primary position"
        doPrint w = prettyPrint w
        words = foldl (++) "" . intersperse " " . filter (not . null) . map doPrint

descender p si = usePointer p "0" (\ x -> rightward x si)

printMachine :: Machine -> IO Machine
printMachine m = do
    putStrLn . prettyPrint $ m
    return m
