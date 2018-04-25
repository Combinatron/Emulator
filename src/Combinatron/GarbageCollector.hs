module Combinatron.GarbageCollector (
    collect,
    initialize,
    printCollector
) where

import Prelude hiding (Word)
import Combinatron.Types hiding (initialize)
import Combinatron.Types.Machine (TaskQueue)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)
import Control.Lens (view, set)
import Data.Maybe (mapMaybe)
import Data.List (sortOn)

initialize = M.empty

printCollector :: Collector -> IO ()
printCollector c = do
    putStr "\nCollector:\t"
    let pairs = sortOn fst (M.toList c)
    mapM_ (\ x -> putStr "\n\t" >> printCollectorEntry x) pairs
    putStr "\n"

printCollectorEntry :: (Pointer, Liveness) -> IO ()
printCollectorEntry (p, l) = do
    putStr $ prettyPrint p
    putStr " - "
    putStr $ show l

type Collector = HashMap Pointer Liveness
type References = [Pointer]

data Liveness = Dead | Referenced | Unreferenced
    deriving (Eq, Show)

collect :: Collector -> Machine -> (Collector, Machine)
collect collector m = (\ (c, si) -> (c, set sentenceIndex si m)) . remove si . mark si . markRoots roots $ collector
    where
        si = view sentenceIndex m
        roots = view nodeRoots m

remove :: SentenceIndex -> Collector -> (Collector, SentenceIndex)
remove si c = (c, V.ifilter (\ i _ -> M.lookupDefault Referenced (newPointer (succ i)) c /= Dead) si)

markRoots :: TaskQueue -> Collector -> Collector
markRoots tq c = foldr (M.update (const (Just Referenced))) c . fmap (view botPointer) $ tasks
    where
        tasks = view taskQueue tq

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
        _ -> foldr (M.update (const (Just Referenced))) updated references
    where
        updated = M.update (const (Just newLiveness)) p c
        liveness = M.lookupDefault Referenced p c
        newLiveness = decreaseLiveness liveness

references :: Sentence -> References
references s = mapMaybe (reference) [fw, sw, tw]
    where
        fw = view priWord s
        sw = view secWord s
        tw = view triWord s

reference :: Word -> Maybe Pointer
reference (N p) = Just p
reference (M p) = Just p
reference _ = Nothing
