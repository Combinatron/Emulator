{
module Combinatron.Parser (parse, Prim(..)) where

import Combinatron.Lexer
import Data.List
}

%name parse_
%tokentype { Token }
%error { parseError }

%token
    basic { Basic _ }
    side { Side _ _ }
    '(' { LParen }
    ')' { RParen }

%%

Expr : {- empty -}  { [] }
     | Expr Basic   { $2 : $1 }
     | Expr Nested  { $2 : $1 }

Basic : basic       { toPrimBasic $1 }
      | side        { toPrimSide $1 }

Nested : '(' Expr ')' { PrimNest (reverse $2) }

{
toPrimBasic (Basic c) = PrimBasic c
toPrimSide (Side c i) = PrimSide c i

data Prim = PrimBasic Char | PrimSide Char Int | PrimNest [Prim]
    deriving (Show)

parseError ts = error $ "Could not parse: " ++ (concat . intersperse " " . map show $ ts)

parse = reverse . parse_
}
