{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TPTP.FormulaParser (
  parseExpr,
) where

import TPTP.FormulaLexer
import TPTP.FormulaSyntax
import qualified Data.List as L
import Control.Monad.Except
}

%name expr
%tokentype { Token }
%monad { Except String } { (>>=) } { return }
%error { parseError }

%token
    biOp       { TokenBiop $$ }
    neg        { TokenNeg }
    word       { TokenWord $$ }
    and        { TokenAnd }
    or         { TokenOr }
    imp        { TokenImp }
    equiv      { TokenEquiv }
    eq         { TokenEq }
    noteq      { TokenNotEq }
    top        { TokenTop }
    bot        { TokenBot }
    rbracket   { TokenRBracket }
    lbracket   { TokenLBracket }
    rrbracket  { TokenRRBracket }
    rlbracket  { TokenRLBracket }
    coron      { TokenCoron }
    comma      { TokenComma }
    all        { TokenAll }
    exists     { TokenExists }

%%

formula
    : word
      { Tletter $1 }
    | lbracket formula imp formula rbracket
      { Tbinary Timp $2 $4 }
    | top
      { Ttrue }
    | bot
      { Tfalse }
    | lbracket formula and trueSubFormula rbracket
      { Tbinary Tand $2 $4 }
    | lbracket formula or trueSubFormula rbracket
      { Tbinary Tor $2 $4 }
    | formula equiv formula
      { Tbinary Tequiv $1 $3 }
    | formula eq formula
      { Tbinary Tequal $1 $3 }
    | formula noteq formula
      { Tneg (Tbinary Tequal $1 $3) }
    | lbracket formula noteq formula rbracket
      { Tneg (Tbinary Tequal $2 $4) }
    | lbracket formula biOp formula rbracket
      { Tbinary Tequiv $2 $4 }
    | lbracket formula rbracket
      { $2 }
    | all rlbracket vars rrbracket coron formula
      { Tall $3 $6 }
    | exists rlbracket vars rrbracket coron formula
      { Texist $3 $6 }
    | formula lbracket srav rbracket
      { TApp $1 $3 }
    | neg formula
      { Tneg $2 }

trueSubFormula
    : formula and trueSubFormula
      { Tbinary Tand $1 $3 }
    | formula or trueSubFormula
      { Tbinary Tor $1 $3 }
    | formula
      { $1 }

var
    : formula coron eq formula
      { [TDef $1 $4] }
    | formula
      { [TFormula $1] }

vars
    : var comma vars
      { $1 ++ $3 }
    | var
      { $1 }

srav
    : var comma srav
      { $3 ++ $1 }
    | var
      { $1 }

{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseExpr :: String -> Either String Expr
parseExpr input =
  let tokenStream = scanTokens input
  in runExcept (expr tokenStream)

}