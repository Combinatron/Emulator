module Combinatron.GarbageCollector (
    collect
) where

import Prelude hiding (Word)
import Combinatron.Types
import Combinatron.Types.Machine (TaskQueue)
import Combinatron.Types.Memory (nullPointer)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M
import Control.Lens (view, set)
import Data.Maybe (mapMaybe)

collect :: Machine -> Machine
collect m = (\ c -> set garbageCollector c m) . mark si . markCursors cursors . markRoots roots $ collector
    where
        si = view sentenceIndex m
        roots = view nodeRoots m
        collector = view garbageCollector m
        cursors = filter (((==) nullPointer) . view cursorPointer) . map (flip view m) $ [botCursor, midCursor, topCursor]

markCursors :: [Cursor] -> Collector -> Collector
markCursors cursors c = foldr markCursor c cursors

markCursor :: Cursor -> Collector -> Collector
markCursor cursor c = withSentence
    where
        withPointer = M.insert p Referenced c
        withSentence = markSentence p (references s) withPointer
        p = view cursorPointer cursor
        s = view cursorSentence cursor

markRoots :: TaskQueue -> Collector -> Collector
markRoots tq c = top
    where
        tasks = view taskQueue tq
        updater p m = foldr (flip M.insert Referenced) m . V.filter (/= nullPointer) . fmap (view p)
        bot = updater botPointer c $ tasks
        mid = updater midPointer bot $ tasks
        top = updater topPointer mid $ tasks

mark :: SentenceIndex -> Collector -> Collector
mark si c = V.ifoldr (\ i s -> markSentence (newPointer (succ i)) (references s)) c si

decreaseLiveness :: Liveness -> Liveness
decreaseLiveness Referenced = Unreferenced
decreaseLiveness Unreferenced = Dead
decreaseLiveness Dead = Dead

markSentence :: Pointer -> References -> Collector -> Collector
markSentence p references c =
    case liveness of
        Dead -> updated
        _ -> foldr (flip M.insert Referenced) updated references
    where
        updated = M.insert p newLiveness c
        liveness = M.lookupDefault Referenced p c
        newLiveness = decreaseLiveness liveness

references :: Sentence -> References
references s = filter (/= nullPointer) . mapMaybe (reference) $ [fw, sw, tw]
    where
        fw = view priWord s
        sw = view secWord s
        tw = view triWord s

reference :: Word -> Maybe Pointer
reference (N p) = Just p
reference (M p) = Just p
reference (Sparked p) = Just p
reference _ = Nothing
