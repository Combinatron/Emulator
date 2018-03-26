module Combinatron.Compiler (
  compile
) where

import Prelude hiding (Word)
import Combinatron.Types
import Combinatron.Parser hiding (Prim(..))
import qualified Combinatron.Parser as P
import Combinatron.Lexer
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)

data Prim = PrimBasic Char | PrimSide Char Int | PrimNest [Prim]
    deriving (Show)

compile :: String -> [Sentence]
compile = primsToSentenceIndex . uncurry collapsePrims . gatherIdentifiers . parse . lexer

gatherIdentifiers :: [Meta] -> (Map Identifier [P.Prim], [P.Prim])
gatherIdentifiers ms = (foldr folder (M.empty) ms, foldr folder2 [] ms)
   where
        -- TODO: The map only ever has PrimNest's in it now.
        folder (Assignment i ps) m = M.insert i [(P.PrimNest ps)] m
        folder _ m = m

        folder2 (Assignment _ _) rest = rest
        folder2 (Prims ps) rest = rest ++ ps

collapsePrims :: Map Identifier [P.Prim] -> [P.Prim] -> [Prim]
collapsePrims idents prims = replaceIdentifiers idents' prims
    where
        idents' = expandIdentifiers idents

expandIdentifiers :: Map Identifier [P.Prim] -> Map Identifier [Prim]
expandIdentifiers m = M.map (expandIdentifier m) m

expandIdentifier :: Map Identifier [P.Prim] -> [P.Prim] -> [Prim]
expandIdentifier _ [] = []
expandIdentifier idents ((P.PrimIdent i):rest) = (expandIdentifier idents (fromJust (M.lookup i idents))) ++ expandIdentifier idents rest
expandIdentifier idents ((P.PrimNest ps):rest) = (PrimNest (expandIdentifier idents ps)):(expandIdentifier idents rest)
expandIdentifier idents (p:rest) = (p2p p):expandIdentifier idents rest

-- Eventually I'll need to do some detection of identifier loops. Something
-- like "If currently replacing identifier X, and I need a lookup for X, fail"
replaceIdentifiers :: Map Identifier [Prim] -> [P.Prim] -> [Prim]
replaceIdentifiers _ [] = []
replaceIdentifiers idents ((P.PrimIdent i):rest) = (fromJust (M.lookup i idents)) ++ replaceIdentifiers idents rest
replaceIdentifiers idents ((P.PrimNest ps):rest) = (PrimNest (replaceIdentifiers idents ps)):replaceIdentifiers idents rest
replaceIdentifiers idents (p:rest) = (p2p p):replaceIdentifiers idents rest

p2p :: P.Prim -> Prim
p2p (P.PrimBasic c) = PrimBasic c
p2p (P.PrimSide c i) = PrimSide c i
p2p _ = error "p2p: unknown P.Prim"

primsToSentenceIndex :: [Prim] -> [Sentence]
primsToSentenceIndex = treeToSentenceIndex . numberNests 2 . toTree

treeToSentenceIndex t = wordsToSentenceIndex (maxNest t) (treeToWords t)

-- Stitch groups of contiguous words into a single sentence index.
-- The groups are grouped into sentences and the first sentence of each group
-- (in order) becomes the first section of the sentence index. The rest of each
-- group is added after the first section in order.
wordsToSentenceIndex :: Int -> [[Word]] -> [Sentence]
wordsToSentenceIndex start contWords  = firsts ++ grouped
    where
        (firsts, grouped) = foldr (grouper (succ start)) ([], []) contWords

grouper :: Int -> [Word] -> ([Sentence], [Sentence]) -> ([Sentence], [Sentence])
grouper start words (firsts, sentences) = (first:firsts, sentences ++ grouped)
     where
        (first:grouped) = groupInSentences (start + (length sentences)) words

groupInSentences :: Int -> [Word] -> [Sentence]
groupInSentences next words
    = case first of
        [] -> case padNulls rest of
                (x:y:z:_) -> [(s (x, y, z))]
                _ -> error "Did not have three terms! This is a bug!"
        _ -> case rest of
                (x:y:z:_) -> (s (n next, y, z)):(groupInSentences (succ next) (first ++ [x]))
                _ -> error "Did not have three terms! This is a bug!"
    where
        (first, rest) = splitAt (length words - 3) words

padNulls :: [Word] -> [Word]
padNulls words = take 3 $ words ++ replicate 3 NullWord

data AnnTree ann = TBasic Char (AnnTree ann) | TSide Char Int (AnnTree ann) | TNest ann (AnnTree ann) (AnnTree ann) | TNull
    deriving (Show)

toTree :: [Prim] -> AnnTree ()
toTree [] = TNull
toTree ((PrimBasic x):prims) = TBasic x (toTree prims)
toTree ((PrimSide x i):prims) = TSide x i (toTree prims)
toTree ((PrimNest p):prims) = TNest () (toTree p) (toTree prims)

-- numbers nest nodes in the tree by essentially making a counter-clockwise
-- "circle" around the tree branches. Numbering the left most branches first,
-- increasing the deeper the node.
numberNests :: Int -> AnnTree () -> AnnTree Int
numberNests _ TNull = TNull
numberNests n (TBasic c t) = TBasic c (numberNests n t)
numberNests n (TSide c i t) = TSide c i (numberNests n t)
numberNests n (TNest _ l r) = TNest n nl (numberNests (succ max') r)
    where
        nl = numberNests (succ n) l
        max' = maxNest nl `max` n

maxNest :: AnnTree Int -> Int
maxNest (TNest i l r) = i `max` maxNest l `max` maxNest r
maxNest (TBasic _ t) = maxNest t
maxNest (TSide _ _ t) = maxNest t
maxNest TNull = 0

-- Takes a tree annotated with the positions of nests and returns a list of
-- continuous word groupings with the nesting words pointing to the right
-- place. The first word group comes first int the final sentence block. The
-- nesting words point to n, where n is 1 + the index of the word group they
-- point to.
treeToWords :: AnnTree Int -> [[Word]]
treeToWords t = right:(concatMap treeToWords lefts)
    where
        right = treeToRWords t
        lefts = treeToLWords t


treeToRWords :: AnnTree Int -> [Word]
treeToRWords (TNest i _ r) = ((n i):treeToRWords r)
treeToRWords t@(TBasic _ r) = ((treeToWord t):treeToRWords r)
treeToRWords t@(TSide _ _ r) = ((treeToWord t):treeToRWords r)
treeToRWords TNull = []

treeToLWords :: AnnTree Int -> [AnnTree Int]
treeToLWords (TNest _ l r) = (l:treeToLWords r)
treeToLWords (TBasic _ r) = treeToLWords r
treeToLWords (TSide _ _ r) = treeToLWords r
treeToLWords _ = []

treeToWord (TBasic 'b' _) = B
treeToWord (TBasic 'c' _) = C
treeToWord (TBasic 'k' _) = K
treeToWord (TBasic 'w' _) = W
treeToWord (TBasic 'y' _) = Y
treeToWord (TBasic 'i' _) = I
treeToWord (TSide 'g' i _) = g i
treeToWord (TSide 'p' i _) = p i
treeToWord (TNest _ _ _) = error "Cannot convert nest directly! This is a bug!"
treeToWord _ = error "Tree node not matched! This is a bug!"
