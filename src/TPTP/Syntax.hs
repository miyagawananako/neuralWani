module TPTP.Syntax where

data Expr
  = Sout String                         -- コメント等
  | File String String                  -- ファイル情報
  | Status String                       -- ステータス
  | PreNum Int                          -- 述語の数
  | ClaNum Int                          -- 節の数
  | Formula String String String String -- Formula 言語 名前 役割 論理式
  | Include String                      -- include文
  deriving (Eq, Show)