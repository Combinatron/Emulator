module Combinatron.Types.Memory where

newtype Pointer = Pointer Int
instance Show Pointer
instance Eq Pointer

newPointer :: Int -> Pointer

indexPower :: Int
