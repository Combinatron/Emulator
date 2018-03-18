{
module Combinatron.Lexer (
    Token(..),
    lexer
) where

import Data.Char (toLower)
}

%wrapper "basic"

$digit = 0-9
$basic = [bckwyi BCKWYI]
$sides = [gp GP]
$comment = \~
$eol = [\n\r]
$nbwhite = $white # $eol
$assignment = \=
$identifier = \:
$assignend = \;
$identifiable = $printable # $identifier

tokens :-
    $eol+         ;
    $nbwhite+     ;
    $basic  { \ (s:[]) -> Basic (toLower s) }
    $sides$digit+ { \ (c:d) -> Side (toLower c) (read d) }
    $comment$printable+$eol ;
    \(  { \ _ -> LParen }
    \)  { \ _ -> RParen }
    $identifier$identifiable+$identifier { \ (_:s) -> Ident (init s) }
    $identifiable+$identifier$assignment { \ s -> Assign (init (init s))}
    $assignend { \ _ -> AssignEnd }

{
data Token
    = Basic Char
    | Side Char Int
    | LParen
    | RParen
    | Ident String
    | Assign String
    | AssignEnd
    deriving (Show)

lexer :: String -> [Token]
lexer = alexScanTokens
}
