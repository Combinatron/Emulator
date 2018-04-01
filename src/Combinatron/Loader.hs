module Combinatron.Loader (
  loadFile
) where

import Prelude hiding (Word)
import Combinatron.Types (Word(..), SentenceIndex, program, g, p, n, m)
import Combinatron.Types.Memory (indexPower)
import qualified Data.Binary.Get as G
import qualified Data.Binary.Strict.BitGet as BG
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as B
import Data.Word hiding (Word)
import Control.Monad (replicateM, when)

data Header = Header Word16
    deriving (Show)

loadHeader :: G.Get Header
loadHeader = do
    cbf <- BC.unpack <$> G.getByteString 3
    when (cbf /= "cbf") (fail "incorrect file format")
    n <- G.getWord16le
    return $ Header n

loadOp :: Word8 -> Word16 -> Word
loadOp 0 = const NullWord
loadOp 1 = const B
loadOp 2 = const C
loadOp 3 = const K
loadOp 4 = const W
loadOp 5 = g . fromIntegral
loadOp 6 = p . fromIntegral
loadOp 7 = n . fromIntegral
loadOp 8 = m . fromIntegral
loadOp 9 = const Y
loadOp 10 = const I
loadOp _ = fail "unrecognized opcode"

loadWord :: BG.BitGet Word
loadWord = do
    opCode <- BG.getAsWord8 4
    pointer <- BG.getAsWord16 indexPower
    return $ loadOp opCode pointer

loadSentence :: G.Get (Word, Word, Word)
loadSentence = do
    sbs <- G.getByteString 6
    let s = BG.runBitGet sbs $ do
            pri <- loadWord
            sec <- loadWord
            tri <- loadWord
            return $ (pri, sec, tri)
    either fail return s

loadSentenceIndex :: G.Get SentenceIndex
loadSentenceIndex = do
    (Header n) <- loadHeader
    sentences <- replicateM (fromIntegral n) loadSentence
    return $ program sentences

loadFile :: B.ByteString -> SentenceIndex
loadFile = G.runGet loadSentenceIndex
