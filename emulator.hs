-- See semantics3

import Prelude hiding (Word(..), return)
import qualified Data.Vector as V
import Data.Monoid
import Data.List (nub)
import Data.Maybe (fromJust)

data WordType = B | C | K | W | G | P | N | M | NullWord
    deriving (Show, Eq)

data Word = Word
    { wordType :: WordType
    , wordAddress :: Maybe Int
    }
    deriving (Show, Eq)

type Cursor = (Maybe Int, Sentence)

type Sentence = (Word, (Word, Word))

data Machine = Machine
    { topCursor :: Cursor
    , midCursor :: Cursor
    , botCursor :: Cursor
    , value :: Maybe Word
    , program :: Program
    }
    deriving (Show)

type Program = V.Vector Sentence

initialize :: Program -> Machine
initialize program = Machine
    -- By convention, the first sentence in the program is the starting point.
    { topCursor = (Just 0, V.head program)
    , midCursor = emptyCursor
    , botCursor = emptyCursor
    , value = Nothing
    , program = program
    }

emptyCursor = (Nothing, emptySentence)
emptySentence = (emptyWord, (emptyWord, emptyWord))
emptyWord = Word NullWord Nothing

prettyPrint :: Machine -> IO ()
prettyPrint machine = do
    putStrLn "Machine"
    putStrLn $ "Cursors:"
    putStrLn $ "- top: " ++ show (topCursor machine)
    putStrLn $ "- mid: " ++ show (midCursor machine)
    putStrLn $ "- bot: " ++ show (botCursor machine)
    putStrLn $ "Value: " ++ show (value machine)
    putStrLn $ "Program: " ++ show (program machine)

nullWord = Word NullWord Nothing
b = Word B Nothing
c = Word C Nothing
k = Word K Nothing
w = Word W Nothing
n addr = Word N (Just addr)
m addr = Word M (Just addr)
g addr = Word G (Just addr)
p addr = Word P (Just addr)

step :: Machine -> Machine
step machine
    | isNest machine = nest machine
    | isReturn machine = return machine
    | isB machine = doB machine
    | isC machine = doC machine
    | isK machine = doK machine
    | isW machine = doW machine

getCurrentWord = fst . snd . topCursor
getCurrentWordType = wordType . getCurrentWord

isNest :: Machine -> Bool
isNest = (N ==) . getCurrentWordType

twoNullSentence (a, (b, c))  = wordType a /= NullWord && wordType b == NullWord && wordType c == NullWord
oneNullSentence (a, (b, c))  = wordType a /= NullWord && wordType b /= NullWord && wordType c == NullWord
zeroNullSentence (a, (b, c)) = wordType a /= NullWord && wordType b /= NullWord && wordType c /= NullWord

isReturn :: Machine -> Bool
isReturn = twoNullSentence . snd . topCursor

isB :: Machine -> Bool
isB = (B ==) . getCurrentWordType

isC :: Machine -> Bool
isC = (C ==) . getCurrentWordType

isK :: Machine -> Bool
isK = (K ==) . getCurrentWordType

isW :: Machine -> Bool
isW = (W ==) . getCurrentWordType

getMaybe :: String -> Maybe a -> a
getMaybe errMessage Nothing = error errMessage
getMaybe _ (Just x) = x

toWriteBack :: Cursor -> (Int, Sentence)
toWriteBack (p, s) = (getMaybe "No address in cursor" p, s)

rotateCursorsUp :: Machine -> Machine
rotateCursorsUp machine = machine'
    where
        machine' = machine { topCursor = (midCursor machine), midCursor = (botCursor machine), botCursor = botCursor', program = program' }
        botCursor' = case pointer of
            (Just x) -> if t == M then (Just x, nestedSentence) else emptyCursor
            _ -> emptyCursor
        nestedSentence = (program machine) V.! (getMaybe "No pointer in M word" $ pointer)
        (Word t pointer) = fst . snd . botCursor $ machine
        program' = (program machine) V.// [toWriteBack . topCursor $ machine]

rotateCursorsDown :: Machine -> Machine
rotateCursorsDown machine = machine'
    where
        machine' = machine { topCursor = topCursor', midCursor = (topCursor machine), botCursor = (midCursor machine), program = program' }
        nestedSentence = (program machine) V.! pointer
        pointer = getMaybe "No pointer in N word" . wordAddress . getCurrentWord $ machine
        topCursor' = (Just pointer, nestedSentence)
        program' = case (fst . botCursor $ machine) of
            (Just _) -> (program machine) V.// [toWriteBack . botCursor $ machine]
            _ -> program machine

nest :: Machine -> Machine
nest machine = machine''
    where
        machine' = rotateCursorsDown machine
        machine'' = machine' { midCursor = midCursor' }
        (botPointer, _) = botCursor machine'
        (midPointer, midSentence) = midCursor machine'
        midCursor' = (midPointer, (Word M $ botPointer, snd midSentence))

return :: Machine -> Machine
return machine = rotateCursorsUp machine'
    where
        machine' = machine { midCursor = midCursor' }
        midCursor' = (midPointer, (getCurrentWord $ machine, snd midSentence))
        (midPointer, midSentence) = midCursor machine

doB :: Machine -> Machine
doB machine
    | isB1 = doB1 machine
    | isB2 = doB2 machine
    | isB3 = doB3 machine
    where
        isB1 = (oneNullSentence . snd . topCursor $ machine) && (zeroNullSentence . snd . midCursor $ machine)
        isB2 = (zeroNullSentence . snd . topCursor $ machine) && (oneNullSentence . snd . midCursor $ machine)
        isB3 = (oneNullSentence . snd . topCursor $ machine) && (oneNullSentence . snd . midCursor $ machine) && (oneNullSentence . snd . botCursor $ machine)

doB1 :: Machine -> Machine
doB1 machine = rotateCursorsUp machine'
    where
        machine' = machine { midCursor = midCursor', program = program' }
        arg1 = fst . snd . snd . topCursor $ machine
        arg2 = fst . snd . snd . midCursor $ machine
        arg3 = snd . snd . snd . midCursor $ machine
        (midPointer, _) = midCursor $ machine
        midCursor' = (midPointer, (arg1, (nested, nullWord)))
        nested = n . V.length . program $ machine
        program' = V.snoc (program machine) (arg2, (arg3, nullWord))

doB2 :: Machine -> Machine
doB2 machine = rotateCursorsUp machine'
    where
        machine' = machine { midCursor = midCursor', program = program' }
        arg1 = fst . snd . snd . topCursor $ machine
        arg2 = snd . snd . snd . topCursor $ machine
        arg3 = fst . snd . snd . midCursor $ machine
        extra = snd . snd . snd . midCursor $ machine
        (midPointer, _) = midCursor $ machine
        midCursor' = (midPointer, (arg1, (nested, extra)))
        nested = n . V.length . program $ machine
        program' = V.snoc (program machine) (arg2, (arg3, nullWord))

doB3 :: Machine -> Machine
doB3 machine = rotateCursorsUp $ rotateCursorsUp machine'
    where
        machine' = machine { botCursor = botCursor', program = program' }
        arg1 = fst . snd . snd . topCursor $ machine
        arg2 = fst . snd . snd . midCursor $ machine
        arg3 = fst . snd . snd . botCursor $ machine
        extra = snd . snd . snd . midCursor $ machine
        (botPointer, _) = botCursor $ machine
        botCursor' = (botPointer, (arg1, (nested, extra)))
        nested = n . V.length . program $ machine
        program' = V.snoc (program machine) (arg2, (arg3, nullWord))

doC :: Machine -> Machine
doC machine
    | isC1 = doC1 machine
    | isC2 = doC2 machine
    | isC3 = doC3 machine
    where
        isC1 = (oneNullSentence . snd . topCursor $ machine) && (zeroNullSentence . snd . midCursor $ machine)
        isC2 = (zeroNullSentence . snd . topCursor $ machine) && (oneNullSentence . snd . midCursor $ machine)
        isC3 = (oneNullSentence . snd . topCursor $ machine) && (oneNullSentence . snd . midCursor $ machine) && (oneNullSentence . snd . botCursor $ machine)

doC1 :: Machine -> Machine
doC1 machine = rotateCursorsUp machine'
    where
        machine' = machine { midCursor = midCursor' }
        arg1 = fst . snd . snd . topCursor $ machine
        arg2 = fst . snd . snd . midCursor $ machine
        arg3 = snd . snd . snd . midCursor $ machine
        (midPointer, _) = midCursor $ machine
        midCursor' = (midPointer, (arg1, (arg3, arg2)))

doC2 :: Machine -> Machine
doC2 machine = rotateCursorsUp machine'
    where
        machine' = machine { midCursor = midCursor', program = program' }
        arg1 = fst . snd . snd . topCursor $ machine
        arg2 = snd . snd . snd . topCursor $ machine
        arg3 = fst . snd . snd . midCursor $ machine
        extra = snd . snd . snd . midCursor $ machine
        (midPointer, _) = midCursor $ machine
        midCursor' = (midPointer, (nested, (arg2, extra)))
        nested = n . V.length . program $ machine
        program' = V.snoc (program machine) (arg1, (arg3, nullWord))

doC3 :: Machine -> Machine
doC3 machine = rotateCursorsUp $ rotateCursorsUp machine'
    where
        machine' = machine { botCursor = botCursor', program = program' }
        arg1 = fst . snd . snd . topCursor $ machine
        arg2 = fst . snd . snd . midCursor $ machine
        arg3 = fst . snd . snd . botCursor $ machine
        extra = snd . snd . snd . midCursor $ machine
        (botPointer, _) = botCursor $ machine
        botCursor' = (botPointer, (nested, (arg2, extra)))
        nested = n . V.length . program $ machine
        program' = V.snoc (program machine) (arg1, (arg3, nullWord))

doK :: Machine -> Machine
doK machine = rotateCursorsUp machine'
    where
        machine' = machine { midCursor = midCursor' }
        arg1 = fst . snd . snd . topCursor $ machine
        arg2 = fst . snd . snd . midCursor $ machine
        extra = snd . snd . snd . midCursor $ machine
        (midPointer, _) = midCursor $ machine
        midCursor' = (midPointer, (arg1, (extra, nullWord)))

doW :: Machine -> Machine
doW machine = rotateCursorsUp machine'
    where
        machine' = machine { midCursor = midCursor', program = program' }
        arg1 = fst . snd . snd . topCursor $ machine
        arg2 = fst . snd . snd . midCursor $ machine
        extra = snd . snd . snd . midCursor $ machine
        (midPointer, _) = midCursor $ machine
        midCursor' = (midPointer, (nested, (arg2, extra)))
        nested = n . V.length . program $ machine
        program' = V.snoc (program machine) (arg1, (arg2, nullWord))

-- Test programs
-- ((K K) K)
nestingProg = V.fromList
    [ (n 1, (k, nullWord))
    , (k, (k, nullWord))
    ]

unnestingProg = V.fromList
    [ (n 1, (k, nullWord))
    , (k, (nullWord, nullWord))
    ]

-- ((B K) K K)
b1Prog = V.fromList
    [ (n 1, (k, k))
    , (b, (k, nullWord))
    ]

-- ((B K K) K)
b2Prog = V.fromList
    [ (n 1, (k, nullWord))
    , (b, (k, k))
    ]

-- (((B K) K) K)
b3Prog = V.fromList
    [ (n 1, (k, nullWord))
    , (n 2, (k, nullWord))
    , (b, (k, nullWord))
    ]

-- ((C K) K K)
c1Prog = V.fromList
    [ (n 1, (b, c))
    , (c, (k, nullWord))
    ]

-- ((C K K) K)
c2Prog = V.fromList
    [ (n 1, (c, nullWord))
    , (c, (k, b))
    ]

-- ((C K) K) K)
c3Prog = V.fromList
    [ (n 1, (c, nullWord))
    , (n 2, (b, nullWord))
    , (c, (k, nullWord))
    ]

-- ((K C) B)
kProg = V.fromList
    [ (n 1, (b, nullWord))
    , (k, (c, nullWord))
    ]

-- ((W C) B)
wProg = V.fromList
    [ (n 1, (b, nullWord))
    , (w, (c, nullWord))
    ]

-- Add programs to test with extra arguments
-- Add more complicated test programs
-- Add more sophisticated test suite
-- Handle G, P instructions
-- Garbage collection
