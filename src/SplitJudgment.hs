{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module SplitJudgment
    ( loadActionsFromBinary
    , getConstantSymbolsFromJudgment
    , getFrequentConstantSymbols
    , Token(..)
    , splitJudgment
    , DelimiterToken(..)
    , dttruleToRuleLabel
    , buildWordMap
    , WordMap
    ) where

import qualified DTS.QueryTypes as QT
import qualified DTS.DTTdeBruijn as U
import qualified DTS.Prover.Wani.BackwardRules as BR
import qualified Data.ByteString as B --bytestring
import qualified Data.Text.Lazy as T  --text
import Data.Store (decode)
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Ord
import qualified Data.Set as Set
import GHC.Generics
import qualified Data.DList as DL
import Data.DList (DList)

loadActionsFromBinary :: FilePath -> IO [(U.Judgment, QT.DTTrule)]
loadActionsFromBinary filepath = do
  binary <- B.readFile filepath
  case decode binary of
    Left peek_exception -> error $ "Could not parse dic file " ++ filepath ++ ": " ++ (show peek_exception)
    Right actions -> return actions

getConstantSymbolsFromPreterm :: U.Preterm -> [T.Text]
getConstantSymbolsFromPreterm (U.Con c) = [c]
getConstantSymbolsFromPreterm (U.Pi a b) = getConstantSymbolsFromPreterm a ++ getConstantSymbolsFromPreterm b
getConstantSymbolsFromPreterm (U.Lam m) = getConstantSymbolsFromPreterm m
getConstantSymbolsFromPreterm (U.App m n) = getConstantSymbolsFromPreterm m ++ getConstantSymbolsFromPreterm n
getConstantSymbolsFromPreterm (U.Not m) = getConstantSymbolsFromPreterm m
getConstantSymbolsFromPreterm (U.Sigma a b) = getConstantSymbolsFromPreterm a ++ getConstantSymbolsFromPreterm b
getConstantSymbolsFromPreterm (U.Pair m n) = getConstantSymbolsFromPreterm m ++ getConstantSymbolsFromPreterm n
getConstantSymbolsFromPreterm (U.Proj _ m) = getConstantSymbolsFromPreterm m
getConstantSymbolsFromPreterm (U.Disj a b) = getConstantSymbolsFromPreterm a ++ getConstantSymbolsFromPreterm b
getConstantSymbolsFromPreterm (U.Iota _ m) = getConstantSymbolsFromPreterm m
getConstantSymbolsFromPreterm (U.Unpack p h m n) = getConstantSymbolsFromPreterm p ++ getConstantSymbolsFromPreterm h ++ getConstantSymbolsFromPreterm m ++ getConstantSymbolsFromPreterm n
getConstantSymbolsFromPreterm (U.Succ n) = getConstantSymbolsFromPreterm n
getConstantSymbolsFromPreterm (U.Natrec p n e f) = getConstantSymbolsFromPreterm p ++ getConstantSymbolsFromPreterm n ++ getConstantSymbolsFromPreterm e ++ getConstantSymbolsFromPreterm f
getConstantSymbolsFromPreterm (U.Eq a m n) = getConstantSymbolsFromPreterm a ++ getConstantSymbolsFromPreterm m ++ getConstantSymbolsFromPreterm n
getConstantSymbolsFromPreterm (U.Refl a m) = getConstantSymbolsFromPreterm a ++ getConstantSymbolsFromPreterm m
getConstantSymbolsFromPreterm (U.Idpeel p e r) = getConstantSymbolsFromPreterm p ++ getConstantSymbolsFromPreterm e ++ getConstantSymbolsFromPreterm r
getConstantSymbolsFromPreterm _ = []

getConstantSymbolsFromPreterms :: [U.Preterm] -> [T.Text]
getConstantSymbolsFromPreterms preterms = concatMap (\preterm -> getConstantSymbolsFromPreterm preterm) preterms

getConstantSymbolsFromSignature :: U.Signature -> [T.Text]
getConstantSymbolsFromSignature signature = concatMap (\(name, preterm) -> [name] ++ getConstantSymbolsFromPreterm preterm) signature

allowDuplicateWords :: Bool
allowDuplicateWords = True

isIncludeTerm :: Bool
isIncludeTerm = False

getConstantSymbolsFromJudgment :: U.Judgment -> [T.Text]
getConstantSymbolsFromJudgment judgment =
  if allowDuplicateWords
    then wordList
    else Set.toList . Set.fromList $ wordList
  where
    wordList =
      if isIncludeTerm
        then  getConstantSymbolsFromSignature (U.signtr judgment) ++
              getConstantSymbolsFromPreterms (U.contxt judgment) ++
              getConstantSymbolsFromPreterm (U.trm judgment) ++
              getConstantSymbolsFromPreterm (U.typ judgment)
        else  getConstantSymbolsFromSignature (U.signtr judgment) ++
              getConstantSymbolsFromPreterms (U.contxt judgment) ++
              getConstantSymbolsFromPreterm (U.typ judgment)

getFrequentConstantSymbols :: [T.Text] -> [T.Text]
getFrequentConstantSymbols frequentWords = take 31 $ map fst $ List.sortOn (Down . snd) $ Map.toList constantSymbolsFreqMap
  where
    constantSymbolsFreqMap :: Map.Map T.Text Int
    constantSymbolsFreqMap = foldr (\word acc -> Map.insertWith (+) word 1 acc) Map.empty frequentWords

data Token =  FST | SND | COMMA | EOPair | EOPre | EOSig | EOCon | EOTerm | EOTyp | LPAREN | RPAREN | SEP
            | Word1 | Word2 | Word3 | Word4 | Word5 | Word6 | Word7 | Word8 | Word9 | Word10 | Word11 | Word12 | Word13 | Word14 | Word15 | Word16 | Word17 | Word18 | Word19 | Word20 | Word21 | Word22 | Word23 | Word24 | Word25 | Word26 | Word27 | Word28 | Word29 | Word30 | Word31 | UNKNOWN
            | Var'0 | Var'1 | Var'2 | Var'3 | Var'4 | Var'5 | Var'6 | Var'unknown
            | Type' | Kind' | Pi' | Lam' | App' | Not' | Sigma' | Pair' | Proj' | Disj' | Iota' | Unpack' | Bot' | Unit' | Top' | Entity' | Nat' | Zero' | Succ' | Natrec' | Eq' | Refl' | Idpeel'
  deriving (Enum, Show, Bounded, Eq, Ord)

-- | テキストをトークンに変換する関数（高速版）
-- WordMapを使用してO(log n)でルックアップ
textToTokenDL :: T.Text -> WordMap -> DList Token
textToTokenDL text wordMap =
  case Map.lookup text wordMap of
    Just token -> DL.singleton token
    Nothing -> DL.singleton UNKNOWN

varToTokenDL :: Int -> DList Token
varToTokenDL i =
  case i of
    0 -> DL.singleton Var'0
    1 -> DL.singleton Var'1
    2 -> DL.singleton Var'2
    3 -> DL.singleton Var'3
    4 -> DL.singleton Var'4
    5 -> DL.singleton Var'5
    6 -> DL.singleton Var'6
    _ -> DL.singleton Var'unknown

selectorToTokenDL :: U.Selector -> DList Token
selectorToTokenDL s = case s of
  U.Fst -> DL.singleton FST
  U.Snd -> DL.singleton SND

data DelimiterToken = Paren | Sep | Eo | Unused deriving (Show, Eq, Enum, Bounded, Ord, Generic, Read)

-- | 頻出語からトークンへのマッピング型
type WordMap = Map.Map T.Text Token

-- | 頻出語リストからWordMapを構築する関数（事前に一度だけ実行）
-- O(n log n)で構築し、後のルックアップをO(log n)にする
buildWordMap :: [T.Text] -> WordMap
buildWordMap frequentWords = 
  Map.fromList $ zip frequentWords wordTokens
  where
    wordTokens = [Word1, Word2, Word3, Word4, Word5, Word6, Word7, Word8, Word9, Word10,
                  Word11, Word12, Word13, Word14, Word15, Word16, Word17, Word18, Word19, Word20,
                  Word21, Word22, Word23, Word24, Word25, Word26, Word27, Word28, Word29, Word30, Word31]

-- | DList版のwrap関数（O(1)での連結）
wrapPretermDL :: DList Token -> DelimiterToken -> DList Token
wrapPretermDL xs Paren = DL.singleton LPAREN <> xs <> DL.singleton RPAREN
wrapPretermDL xs Sep = xs <> DL.singleton SEP
wrapPretermDL xs Eo = xs <> DL.singleton EOPre
wrapPretermDL xs Unused = xs

-- | DList版のsplitPreterm（O(1)での連結）
splitPretermDL :: U.Preterm -> WordMap -> DelimiterToken -> DList Token
splitPretermDL preterm wordMap delimiterToken = case preterm of
  U.Var i -> wrapPretermDL (varToTokenDL i) delimiterToken
  U.Con c -> wrapPretermDL (textToTokenDL c wordMap) delimiterToken
  U.Type -> wrapPretermDL (DL.singleton Type') delimiterToken
  U.Kind -> wrapPretermDL (DL.singleton Kind') delimiterToken
  U.Pi a b -> wrapPretermDL (DL.singleton Pi' <> splitPretermDL a wordMap delimiterToken <> splitPretermDL b wordMap delimiterToken) delimiterToken
  U.Lam m -> wrapPretermDL (DL.singleton Lam' <> splitPretermDL m wordMap delimiterToken) delimiterToken
  U.App m n -> wrapPretermDL (DL.singleton App' <> splitPretermDL m wordMap delimiterToken <> splitPretermDL n wordMap delimiterToken) delimiterToken
  U.Not m -> wrapPretermDL (DL.singleton Not' <> splitPretermDL m wordMap delimiterToken) delimiterToken
  U.Sigma a b -> wrapPretermDL (DL.singleton Sigma' <> splitPretermDL a wordMap delimiterToken <> splitPretermDL b wordMap delimiterToken) delimiterToken
  U.Pair m n -> wrapPretermDL (DL.singleton Pair' <> splitPretermDL m wordMap delimiterToken <> splitPretermDL n wordMap delimiterToken) delimiterToken
  U.Proj s m -> wrapPretermDL (DL.singleton Proj' <> selectorToTokenDL s <> splitPretermDL m wordMap delimiterToken) delimiterToken
  U.Disj a b -> wrapPretermDL (DL.singleton Disj' <> splitPretermDL a wordMap delimiterToken <> splitPretermDL b wordMap delimiterToken) delimiterToken
  U.Iota s m -> wrapPretermDL (DL.singleton Iota' <> selectorToTokenDL s <> splitPretermDL m wordMap delimiterToken) delimiterToken
  U.Unpack p h m n -> wrapPretermDL (DL.singleton Unpack' <> splitPretermDL p wordMap delimiterToken <> splitPretermDL h wordMap delimiterToken <> splitPretermDL m wordMap delimiterToken <> splitPretermDL n wordMap delimiterToken) delimiterToken
  U.Bot -> wrapPretermDL (DL.singleton Bot') delimiterToken
  U.Unit -> wrapPretermDL (DL.singleton Unit') delimiterToken
  U.Top -> wrapPretermDL (DL.singleton Top') delimiterToken
  U.Entity -> wrapPretermDL (DL.singleton Entity') delimiterToken
  U.Nat -> wrapPretermDL (DL.singleton Nat') delimiterToken
  U.Zero -> wrapPretermDL (DL.singleton Zero') delimiterToken
  U.Succ m -> wrapPretermDL (DL.singleton Succ' <> splitPretermDL m wordMap delimiterToken) delimiterToken
  U.Natrec p n e f -> wrapPretermDL (DL.singleton Natrec' <> splitPretermDL p wordMap delimiterToken <> splitPretermDL n wordMap delimiterToken <> splitPretermDL e wordMap delimiterToken <> splitPretermDL f wordMap delimiterToken) delimiterToken
  U.Eq a m n -> wrapPretermDL (DL.singleton Eq' <> splitPretermDL a wordMap delimiterToken <> splitPretermDL m wordMap delimiterToken <> splitPretermDL n wordMap delimiterToken) delimiterToken
  U.Refl a m -> wrapPretermDL (DL.singleton Refl' <> splitPretermDL a wordMap delimiterToken <> splitPretermDL m wordMap delimiterToken) delimiterToken
  U.Idpeel p e r -> wrapPretermDL (DL.singleton Idpeel' <> splitPretermDL p wordMap delimiterToken <> splitPretermDL e wordMap delimiterToken <> splitPretermDL r wordMap delimiterToken) delimiterToken

-- | DList版のsplitPreterms
splitPretermsDL :: [U.Preterm] -> WordMap -> DelimiterToken -> DList Token
splitPretermsDL preterms wordMap delimiterToken = 
  mconcat $ map (\preterm -> splitPretermDL preterm wordMap delimiterToken) preterms

-- | DList版のwrapPair
wrapPairDL :: DList Token -> DelimiterToken -> DList Token
wrapPairDL xs Paren = DL.singleton LPAREN <> xs <> DL.singleton RPAREN
wrapPairDL xs Sep = xs <> DL.singleton SEP
wrapPairDL xs Eo = xs <> DL.singleton EOPair
wrapPairDL xs Unused = xs

-- | DList版のsplitSignature
splitSignatureDL :: U.Signature -> WordMap -> DelimiterToken -> DList Token
splitSignatureDL signature wordMap delimiterToken = 
  mconcat $ map (\(name, preterm) -> 
    wrapPairDL (textToTokenDL name wordMap <> DL.singleton COMMA <> splitPretermDL preterm wordMap delimiterToken) delimiterToken
  ) signature

-- | DList版のwrapSignature
wrapSignatureDL :: DList Token -> DelimiterToken -> DList Token
wrapSignatureDL xs Paren = DL.singleton LPAREN <> xs <> DL.singleton RPAREN
wrapSignatureDL xs Sep = xs <> DL.singleton SEP
wrapSignatureDL xs Eo = xs <> DL.singleton EOSig
wrapSignatureDL xs Unused = xs

-- | DList版のwrapContext
wrapContextDL :: DList Token -> DelimiterToken -> DList Token
wrapContextDL xs Paren = DL.singleton LPAREN <> xs <> DL.singleton RPAREN
wrapContextDL xs Sep = xs <> DL.singleton SEP
wrapContextDL xs Eo = xs <> DL.singleton EOCon
wrapContextDL xs Unused = xs

-- | DList版のwrapTerm
wrapTermDL :: DList Token -> DelimiterToken -> DList Token
wrapTermDL xs Paren = DL.singleton LPAREN <> xs <> DL.singleton RPAREN
wrapTermDL xs Sep = xs <> DL.singleton SEP
wrapTermDL xs Eo = xs <> DL.singleton EOTerm
wrapTermDL xs Unused = xs

-- | DList版のwrapTyp
wrapTypDL :: DList Token -> DelimiterToken -> DList Token
wrapTypDL xs Paren = DL.singleton LPAREN <> xs <> DL.singleton RPAREN
wrapTypDL xs Sep = xs <> DL.singleton SEP
wrapTypDL xs Eo = xs <> DL.singleton EOTyp
wrapTypDL xs Unused = xs

-- | DList版のsplitJudgment（内部実装）
splitJudgmentDL :: U.Judgment -> WordMap -> DelimiterToken -> DList Token
splitJudgmentDL judgment wordMap delimiterToken =
  if isIncludeTerm
    then  wrapSignatureDL (splitSignatureDL (U.signtr judgment) wordMap delimiterToken) delimiterToken <>
          wrapContextDL (splitPretermsDL (U.contxt judgment) wordMap delimiterToken) delimiterToken <>
          wrapTermDL (splitPretermDL (U.trm judgment) wordMap delimiterToken) delimiterToken <>
          wrapTypDL (splitPretermDL (U.typ judgment) wordMap delimiterToken) delimiterToken
    else  wrapSignatureDL (splitSignatureDL (U.signtr judgment) wordMap delimiterToken) delimiterToken <>
          wrapContextDL (splitPretermsDL (U.contxt judgment) wordMap delimiterToken) delimiterToken <>
          wrapTypDL (splitPretermDL (U.typ judgment) wordMap delimiterToken) delimiterToken

-- | splitJudgment（公開インターフェース）
-- 内部でDListを使用し、最後にリストに変換
splitJudgment :: U.Judgment -> WordMap -> DelimiterToken -> [Token]
splitJudgment judgment wordMap delimiterToken = 
  DL.toList $ splitJudgmentDL judgment wordMap delimiterToken

-- | DTTruleからRuleLabelへの変換関数
-- lightblueのBR.dttruleToRuleLabelをエクスポート
dttruleToRuleLabel :: QT.DTTrule -> Maybe BR.RuleLabel
dttruleToRuleLabel = BR.dttruleToRuleLabel
