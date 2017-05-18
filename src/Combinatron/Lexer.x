{
module Combinatron.Lexer (
    Token(..),
    lexer
) where

import Data.Char (toLower)
}

%wrapper "basic"

$digit = 0-9
$basic = [bckwy BCKWY]
$sides = [gp GP]
$eol = [\n]

tokens :-
    $eol        ;
    $white+     ;
    $basic  { \ (s:[]) -> Basic (toLower s) }
    $sides$digit+ { \ (c:d) -> Side (toLower c) (read d) }
    \(  { \ _ -> LParen }
    \)  { \ _ -> RParen }

{
data Token
    = Basic Char
    | Side Char Int
    | LParen
    | RParen
    deriving (Show)

lexer :: String -> [Token]
lexer = alexScanTokens
}
