{
module Combinatron.Parser (parse, Prim(..), Meta(..), Identifier(..)) where

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
    assign { Assign _ }
    ngissa { AssignEnd }
    identifier { Ident _ }

%%

Expr : {- empty -} { [] }
     | Expr Meta { $2:$1 }

Meta : Assignment { $1 }
     | RHS { (Prims (reverse $1)) }

Assignment : assign RHS ngissa { Assignment (aToIdentifier $1) (reverse $2) }

RHS : {- empty -}  { [] }
    | RHS Basic    { $2 : $1 }
    | RHS Nested   { $2 : $1 }
    | RHS identifier { (PrimIdent (toIdentifier $2)) : $1 }

Basic : basic       { toPrimBasic $1 }
      | side        { toPrimSide $1 }

Nested : '(' RHS ')' { PrimNest (reverse $2) }

{
toPrimBasic (Basic c) = PrimBasic c
toPrimSide (Side c i) = PrimSide c i

aToIdentifier (Assign s) = Identifier s
toIdentifier (Ident s) = Identifier s

data Prim = PrimBasic Char | PrimSide Char Int | PrimNest [Prim] | PrimIdent Identifier
    deriving (Show)

data Meta = Assignment Identifier [Prim] | Prims [Prim]
    deriving (Show)

newtype Identifier = Identifier String
    deriving (Show, Eq, Ord)

parseError ts = error $ "Could not parse: " ++ (concat . intersperse " " . map show $ ts)

parse = reverse . parse_
}
