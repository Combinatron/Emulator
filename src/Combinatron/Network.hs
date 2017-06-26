{-# LANGUAGE RankNTypes #-}
module Combinatron.Network where

import System.Random (randomRIO, randomIO)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TQueue
import Control.Monad.STM
import Control.Monad (forever, mapM_)
import Data.Tuple (swap)
import qualified Data.ByteString.Char8 as BS

-- Chips have a step function that either transitions them to their next state
-- or halts the machine.
class Chip a where
    step :: a -> Either a a

newtype MachineId = MachineId Int

instance Show MachineId where
    show (MachineId x) = show x

-- Terminals are a Chip that generates output to write to the outgoing queue
-- and reads inputs from the incoming queue.
data Terminal a = Terminal
    { terminalMachine :: (MachineId, a)
    -- outgoing queue is always written to by the terminal and read by the wire
    , outgoing :: TQueue BS.ByteString
    -- incoming queue is always read by the terminal and written to by the wire
    , incoming :: TQueue BS.ByteString
    }

terminal :: a -> MachineId -> IO (Terminal a)
terminal m id = do
    inc <- newTQueueIO
    out <- newTQueueIO
    let t = (Terminal (id, m) out inc)
    forkIO (runTerminal t)
    return t

runTerminal :: Terminal a -> IO ()
runTerminal t = forever $ do
    -- do some work
    -- step (snd (terminalMachine t))
    -- spark a task
    atomically $ do
        writeTQueue (outgoing t) (BS.pack "message")
    -- check for new tasks
    atomically $ do
        tryReadTQueue (incoming t)

-- Simulate a uni-directional wire with given properties
-- If the producers and consumers are clear, I'd eventually like to get rid of
-- the STM primitives. The main thing they are buying me now is blocking reads
-- so the simulation doesn't peg my CPU. That may just be a simulation problem
-- though and not something I'd encounter in hardware.
data Wire a b = Wire
    { left   :: Terminal a
    , right  :: Terminal b
    }

wire :: WireSide a b c -> WireSide a b d -> WireMod -> Wire a b -> IO ()
wire  inc out wireMod w = forever $ do
    m <- atomically $ readTQueue $ outgoing $ inc $ w
    m' <- wireMod m
    case m' of
        (Just m) -> atomically $ writeTQueue (incoming $ out $ w) m
        Nothing -> return ()

type WireSide a b c = Wire a b -> Terminal c

-- Wire Modifiers
type WireMod = (BS.ByteString -> IO (Maybe BS.ByteString))
-- No modifications to the wire properties, effectively "instant" transmission
plain m = return (Just m)
-- A random amount of latency within some bounds
latency low high m = do
    delay <- randomRIO (low, high)
    threadDelay delay
    return (Just m)
-- Randomly drop packets according to some chance
dropped chance m = do
    skip <- fmap ((<=) chance) ((randomRIO (0, 1)) :: IO Float)
    if skip
    then return Nothing
    else return (Just m)
-- Combine latency and dropped connections
latencyAndDropped low high chance m = do
    m' <- latency low high m
    maybe (return Nothing) (dropped chance) m'

-- Directional wires
wireLeft = wire left right
wireRight = wire right left

-- Build a network of Wires and Terminals
type Network a b = [Wire a b]

addWire :: forall a b . MVar (Network a b) -> Wire a b -> IO ()
addWire n c = do
    forkIO (wireLeft plain c)
    forkIO (wireRight plain c)
    modifyMVar_ n (return . (:) c)

network :: forall a . [(MachineId, a)] -> [(Int, Int)] -> IO (MVar (Network a a))
network machines wireSpecs = do
    terms <- terminals machines
    let terminalWires = map (\ (l, r) -> (terms !! l, terms !! r)) wireSpecs
        wires = map (uncurry Wire) terminalWires
    n <- newMVar []
    -- I don't really need this MVar
    mapM_ (addWire n) wires
    return n

terminals = mapM (uncurry terminal . swap)

-- Testing

instance Chip () where
    step = undefined

testMachines :: [(MachineId, ())]
testMachines =
    [(MachineId 0, ())
    ,(MachineId 1, ())
    ,(MachineId 2, ())
    ,(MachineId 3, ())
    ,(MachineId 4, ())
    ]

testWires =
    [(0, 1)
    ,(1, 2)
    ,(1, 3)
    ,(2, 4)
    ]

testNetwork = network testMachines testWires
