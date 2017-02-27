module Combinatron.Programs where

import Combinatron.Types

-- | Test programs
-- ((K K) K)
nestingProg = program
    [ (n 2, K, NullWord)
    , (K, K, NullWord)
    ]

unnestingProg = program
    [ (n 2, K, NullWord)
    , (K, NullWord, NullWord)
    ]

-- ((B K) C W) -> K (C W)
b2Prog = program
    [ (n 2, C, W)
    , (B, K, NullWord)
    ]

-- ((B K C) B W) -> K (C B) W
b1Prog = program
    [ (n 2, B, W)
    , (B, K, C)
    ]

-- (((B K) C) W B) -> K (C W) B
b3Prog = program
    [ (n 2, W, B)
    , (n 3, C, NullWord)
    , (B, K, NullWord)
    ]

-- ((C K) B C) -> K C B
c2Prog = program
    [ (n 2, B, C)
    , (C, K, NullWord)
    ]

-- ((C K B) C W) -> K C B W
c1Prog = program
    [ (n 2, C, W)
    , (C, K, B)
    ]

-- ((C K) B) C W) -> K C B W
c3Prog = program
    [ (n 2, C, W)
    , (n 3, B, NullWord)
    , (C, K, NullWord)
    ]

-- ((K C) B B) -> C B
k2Prog = program
    [ (n 2, B, B)
    , (K, C, NullWord)
    ]

-- (K C B) -> C
k1Prog = program
    [ (K, C, B)
    ]

-- ((W C) B) -> C B B
w2Prog = program
    [ (n 2, B, K)
    , (W, C, NullWord)
    ]

w1Prog = program
    [ (W, C, B)
    ]
