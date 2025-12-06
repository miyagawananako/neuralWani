module TPTP.FormulaSyntax where

data Tvar =
    TDef Expr Expr      -- 型付き変数 (X : type)
  | TFormula Expr       -- 単純な変数
  deriving (Eq, Show)

data Tbop = Tand | Tor | Timp | Tequiv | Tequal
  deriving (Eq, Show)

data Expr =
    Tletter String          -- 述語/定数
  | Ttrue                   -- $true
  | Tfalse                  -- $false
  | Tneg Expr               -- ~φ
  | Tbinary Tbop Expr Expr  -- φ ∘ ψ
  | Tall [Tvar] Expr        -- ![X]: φ
  | Texist [Tvar] Expr      -- ?[X]: φ
  | TApp Expr [Tvar]        -- f(a,b,...)
  deriving (Eq, Show)