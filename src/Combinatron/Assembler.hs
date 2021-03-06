module Combinatron.Assembler (
  assembleFile
) where

import Prelude hiding (Word)
import Combinatron.Types (Word(..), SentenceIndex, Sentence(..))
import Combinatron.Types.Memory (Pointer(..))
import Combinatron.Types.Instructions (wordArgSize)
import qualified Data.Binary.Put as P
import qualified Data.Binary.BitPut as BP
import qualified Data.ByteString.Char8 as BC
import Data.Word hiding (Word)
import Control.Monad (forM_)
import qualified Data.Vector as V

assembleHeader :: Word16 -> P.Put
assembleHeader n = do
    P.putByteString (BC.pack "cbf")
    P.putWord16le n

assembleFile s = P.runPut (assembleProgram s)

assembleProgram :: SentenceIndex -> P.Put
assembleProgram s = do
    assembleHeader (fromIntegral $ V.length s)
    P.putLazyByteString $
        BP.runBitPut $ forM_ s assembleSentence

assembleSentence :: Sentence -> BP.BitPut
assembleSentence s = do
    assembleOp (_priWord s)
    assembleOp (_secWord s)
    assembleOp (_triWord s)

assembleOp :: Word -> BP.BitPut
assembleOp w = do
    BP.putNBits 4 $ opCode w
    BP.putNBits (wordArgSize w) $ opPointer w

opCode :: Word -> Word8
opCode NullWord = 0
opCode B = 1
opCode C = 2
opCode K = 3
opCode W = 4
opCode (G _) = 5
opCode (P _) = 6
opCode (N _) = 7
opCode (M _) = 8
opCode Y = 9
opCode I = 10
opCode (Sparked _) = 11

opPointer :: Word -> Word16
opPointer (N (Pointer p)) = fromIntegral p
opPointer (M (Pointer p)) = fromIntegral p
opPointer (G (Pointer p)) = fromIntegral p
opPointer (P (Pointer p)) = fromIntegral p
opPointer (Sparked (Pointer p)) = fromIntegral p
opPointer _ = 0
