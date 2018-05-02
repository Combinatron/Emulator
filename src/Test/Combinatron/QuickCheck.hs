{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving, RankNTypes #-}
module Test.Combinatron.QuickCheck where

import Prelude hiding (Word)
import Test.QuickCheck
import Combinatron.Types
import Combinatron (runN, megacycle)
import Combinatron.Reducer (ExecutionStep(..), unwrapExecutionStep)
import qualified Data.Vector as V
import Data.List (partition, nub)
import Control.Lens (Lens', set)

-- QuickCheck Instances
instance Arbitrary Word where
    arbitrary = do
        i <- resize 8 (arbitrarySizedNatural :: Gen Int)
        case i of
            0 -> return B
            1 -> return C
            2 -> return K
            3 -> return W
            4 -> return NullWord
            -- Pointers must be greater than 0
            -- No M word is generated here because those are only generated by operation of the machine
            -- No Sparked word is generated here because those are only generated by operation of the machine
            5 -> n . succ <$> arbitrarySizedNatural
            6 -> g . succ <$> arbitrarySizedNatural
            7 -> p . succ <$> arbitrarySizedNatural
            8 -> return I
            _ -> error "Generated index higher than 8! This should not happen!"

instance Arbitrary Sentence where
    arbitrary = do
        (x, y, z) <- (,,) <$> arbitrary <*> arbitrary <*> arbitrary
        let (nulls, notnulls) = partition isNull [x, y, z]
            (x':y':z':[]) = notnulls ++ nulls
        return $ Sentence x' y' z'

instance Arbitrary SentenceIndex where
    arbitrary = V.fromList <$> listOf arbitrary

-- Generate arbitrary machines by generating arbitrary programs
instance Arbitrary Machine where
    arbitrary = do
        program <- arbitrary :: Gen SentenceIndex
        return $ initialize program

newtype NonEmptySentenceIndex = NonEmptySentenceIndex SentenceIndex
    deriving (Show)

instance Arbitrary NonEmptySentenceIndex where
    arbitrary = NonEmptySentenceIndex . V.fromList <$> listOf1 arbitrary

data CursorSelection = CursorSelection String (Lens' Machine Cursor)

instance Show CursorSelection where
    show (CursorSelection s _) = "{CursorSelection " ++ s ++ "}"

instance Arbitrary CursorSelection where
    arbitrary = elements $ [CursorSelection "botCursor" botCursor, CursorSelection "midCursor" midCursor, CursorSelection "topCursor" topCursor]

newtype UniqueSentenceIndex = UniqueSentenceIndex SentenceIndex
    deriving (Show)

instance Arbitrary UniqueSentenceIndex where
    arbitrary = UniqueSentenceIndex . V.fromList . nub <$> listOf1 arbitrary

newtype SteppedMachine = SteppedMachine Machine
    deriving (Show)

-- FIXME: Generate machines that end up with words in multiple cursors
instance Arbitrary SteppedMachine where
    arbitrary = do
        (ValidProgram program) <- arbitrary :: Gen ValidProgram
        steps <- resize (V.length program) (arbitrarySizedNatural :: Gen Int)
        return $ SteppedMachine $ unwrapExecutionStep $ runN megacycle steps $ Initialized (initialize program)

-- Valid programs must have no repeated sentences and all pointers must be
-- within the length of the program.
newtype ValidProgram = ValidProgram SentenceIndex
    deriving (Show)

instance Arbitrary ValidProgram where
    arbitrary = do
        (NonEmptySentenceIndex prog) <- arbitrary :: Gen NonEmptySentenceIndex
        let l = V.length uProg
            uProg = V.fromList . nub . V.toList $ prog
            vProg = V.map mapper uProg
            mapper (Sentence x y z) = Sentence (fixer x) (fixer y) (fixer z)
            fixer e = case e of
                (N p) -> n (usePointer p 0 ((+1) . flip mod l))
                (M p) -> m (usePointer p 0 ((+1) . flip mod l))
                (G p) -> g (usePointer p 0 ((+1) . flip mod l))
                (P p') -> p (usePointer p' 0 ((+1) . flip mod l))
                x -> x
        return $ ValidProgram vProg

data WordSelection = WordSelection String (Lens' Sentence Word)

instance Show WordSelection where
    show (WordSelection s _) = "{WordSelection " ++ s ++ "}"

instance Arbitrary WordSelection where
    arbitrary = elements $ [WordSelection "priWord" priWord, WordSelection "secWord" secWord, WordSelection "triWord" triWord]

data CWSelection = CWSelection CursorSelection WordSelection
    deriving (Show)

instance Arbitrary CWSelection where
    arbitrary = CWSelection <$> arbitrary <*> arbitrary

toLens (CWSelection (CursorSelection _ c) (WordSelection _ w)) = c.cursorSentence.w

data MachineWithValue = MachineWithValue Machine
    deriving (Show)

instance Arbitrary MachineWithValue where
    arbitrary = do
        v <- arbitrary
        (NonEmptySentenceIndex p) <- arbitrary
        return $ MachineWithValue $ set value v $ initialize p
