module Combinatron.Loader (
  loadFile
) where

import Prelude hiding (Word)
import Combinatron.Types (Word(..), SentenceIndex, program, g, p, n, m, sparked)
import Combinatron.Types.Instructions (wordArgSize)
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
loadOp 11 = sparked . fromIntegral
loadOp _ = fail "unrecognized opcode"

dummy = 1

loadWord :: BG.BitGet Word
loadWord = do
    opCode <- BG.getAsWord8 4
    let wOp = loadOp opCode dummy
    pointer <- BG.getAsWord16 (wordArgSize wOp)
    return $ loadOp opCode pointer

loadSentence :: BG.BitGet (Word, Word, Word)
loadSentence = do
    pri <- loadWord
    sec <- loadWord
    tri <- loadWord
    return $ (pri, sec, tri)

loadSentenceIndex :: Word16 -> B.ByteString -> Either String SentenceIndex
loadSentenceIndex n s = BG.runBitGet (B.toStrict s) $ do
    sentences <- replicateM (fromIntegral n) loadSentence
    return $ program sentences

loadFile :: B.ByteString -> SentenceIndex
loadFile bs = si
    where
        (rest, _, (Header n)) =
            case G.runGetOrFail loadHeader bs of
                Right x -> x
                Left (_, _, s) -> error s
        si =
            case loadSentenceIndex n rest of
                Right x -> x
                Left s -> error s
