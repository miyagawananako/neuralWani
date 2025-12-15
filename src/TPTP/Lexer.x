{
module TPTP.Lexer (
  Token(..),
  scanTokens
) where

import TPTP.Syntax
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]
$space = [\ ]
$word = [A-Za-z $digit \_ \' \- \+ \[ \] \! \? \; \= \$ \" \/ \{ \} \* \< \> \^ \\]

tokens :-
  $eol                          ;
  $white+                       ;
  \&                            { \s -> TokenAnd }
  \)                            { \s -> TokenRBracket }
  \(                            { \s -> TokenLBracket }
  \]                            { \s -> TokenRRBracket }
  \[                            { \s -> TokenRLBracket }
  \~                            { \s -> TokenConne s }
  \|                            { \s -> TokenConne s }
  \< \= \>                      { \s -> TokenConne s }
  \= \>                         { \s -> TokenConne s }
  \< \~ \>                      { \s -> TokenConne s }
  \~ \|                         { \s -> TokenConne s }
  \~ \&                         { \s -> TokenConne s }
  \,                            { \s -> TokenComma }
  \.                            { \s -> TokenPeriod }
  fof                           { \s -> TokenFOF }
  cnf                           { \s -> TokenCNF }
  [$space]* $digit+             { \s -> TokenNum (read s) }
  include                       { \s -> TokenInclude }
  \% [$space]* Status           { \s -> TokenStatus }
  \% [$space]* Number [$space]+ of [$space]+ predicates
                                { \s -> TokenPreNum }
  [$space]* \%                  { \s -> TokenHead }
  [$space]* \:                  { \s -> TokenCoron }
  [$word]+                      { \s -> TokenWord s }

{

data Token
  = TokenNum Int
  | TokenPreNum
  | TokenClause
  | TokenFOF
  | TokenCNF
  | TokenHead
  | TokenCoron
  | TokenComma
  | TokenInclude
  | TokenConne String
  | TokenWord String
  | TokenStatus
  | TokenAnd
  | TokenRBracket
  | TokenLBracket
  | TokenRRBracket
  | TokenRLBracket
  | TokenPeriod
  deriving (Eq, Show)

scanTokens = alexScanTokens

}