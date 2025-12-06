module TPTPInfo where

import qualified DTS.DTTdeBruijn as DT
import Data.Default (Default(..))

data Language = THF | TFF | FOF | CNF
  deriving (Eq)

instance Show Language where
  show THF = "thf"
  show TFF = "tff"
  show FOF = "fof"
  show CNF = "cnf"

instance Read Language where
  readsPrec _ str =
    let lans = [THF, TFF, FOF, CNF]
    in map (\(la, (s, r)) -> (la, r))
       $ filter (\(la, (s, r)) -> show la == s)
       $ zip lans $ map (\lan -> splitAt (length (show lan)) str) lans

data Role = Axiom | Hypothesis | Definition | Assumption 
          | Lemma | RTheorem | Corollary | Conjecture 
          | NegatedConjecture | Plain | RType | RUnknown
  deriving (Eq)

instance Show Role where
  show Axiom = "axiom"
  show Hypothesis = "hypothesis"
  show Assumption = "assumption"
  show Lemma = "lemma"
  show RTheorem = "theorem"
  show Corollary = "corollary"
  show Conjecture = "conjecture"
  show NegatedConjecture = "negated_conjecture"
  show Plain = "plain"
  show RType = "type"
  show RUnknown = "unknown"
  show Definition = "definition"

instance Read Role where
  readsPrec _ str =
    let roles = [Axiom, Hypothesis, Assumption, Lemma, RTheorem, 
                 Corollary, Conjecture, NegatedConjecture, Plain, RType, RUnknown]
    in map (\(ro, (s, r)) -> (ro, r))
       $ filter (\(ro, (s, r)) -> show ro == s)
       $ zip roles $ map (\role -> splitAt (length (show role)) str) roles

isAxiomLike :: Role -> Bool
isAxiomLike role = role `elem` [Axiom, Hypothesis, Definition, Assumption, Lemma, RTheorem, Corollary]

data Status = Theorem | ContradictoryAxioms | Satisfiable 
            | Unsatisfiable | CounterSatisfiable | Unknown | Open
  deriving (Show, Read, Eq)

data Result = YES | NO | UNKNOWN
  deriving (Show, Read, Eq, Enum, Bounded)

statusToResult :: Status -> Result
statusToResult status
  | status `elem` [Theorem, ContradictoryAxioms] = YES
  | status `elem` [CounterSatisfiable, Unsatisfiable] = NO
  | otherwise = UNKNOWN

data Info = Info
  { language   :: Maybe Language
  , status     :: Maybe Status
  , filename   :: String
  , result     :: Result
  , note       :: String
  , context    :: [DT.Preterm]       -- 公理のリスト
  , target     :: Maybe DT.Preterm   -- 証明すべき命題
  , signature  :: DT.Signature       -- シグネチャ
  , prelst     :: [(String, Int)]    -- 述語リスト
  , strcontext :: String             -- 元の文字列（デバッグ用）
  , strtarget  :: String             -- 元の文字列（デバッグ用）
  } deriving (Show, Eq)

instance Default Info where
  def = Info
    { language = Nothing
    , status = Nothing
    , filename = ""
    , result = UNKNOWN
    , note = ""
    , context = []
    , target = Nothing
    , signature = []
    , prelst = []
    , strcontext = ""
    , strtarget = ""
    }