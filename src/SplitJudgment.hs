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
textToToken :: T.Text -> WordMap -> [Token]
textToToken text wordMap =
  case Map.lookup text wordMap of
    Just token -> [token]
    Nothing -> [UNKNOWN]

varToToken :: Int -> [Token]
varToToken i =
  case i of
    0 -> [Var'0]
    1 -> [Var'1]
    2 -> [Var'2]
    3 -> [Var'3]
    4 -> [Var'4]
    5 -> [Var'5]
    6 -> [Var'6]
    _ -> [Var'unknown]

selectorToToken :: U.Selector -> [Token]
selectorToToken s = case s of
  U.Fst -> [FST]
  U.Snd -> [SND]

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

wrapPreterm :: [Token] -> DelimiterToken -> [Token]
wrapPreterm xs Paren = [LPAREN] ++ xs ++ [RPAREN]
wrapPreterm xs Sep = xs ++ [SEP]
wrapPreterm xs Eo = xs ++ [EOPre]
wrapPreterm xs Unused = xs

splitPreterm :: U.Preterm -> WordMap -> DelimiterToken -> [Token]
splitPreterm preterm wordMap delimiterToken = case preterm of
  U.Var i -> wrapPreterm (varToToken i) delimiterToken
  U.Con c -> wrapPreterm (textToToken c wordMap) delimiterToken
  U.Type -> wrapPreterm [Type'] delimiterToken
  U.Kind -> wrapPreterm [Kind'] delimiterToken
  U.Pi a b -> wrapPreterm ([Pi'] ++ splitPreterm a wordMap delimiterToken ++ splitPreterm b wordMap delimiterToken) delimiterToken
  U.Lam m -> wrapPreterm ([Lam'] ++ splitPreterm m wordMap delimiterToken) delimiterToken
  U.App m n -> wrapPreterm ([App'] ++ splitPreterm m wordMap delimiterToken ++ splitPreterm n wordMap delimiterToken) delimiterToken
  U.Not m -> wrapPreterm ([Not'] ++ splitPreterm m wordMap delimiterToken) delimiterToken
  U.Sigma a b -> wrapPreterm ([Sigma'] ++ splitPreterm a wordMap delimiterToken ++ splitPreterm b wordMap delimiterToken) delimiterToken
  U.Pair m n -> wrapPreterm ([Pair'] ++ splitPreterm m wordMap delimiterToken ++ splitPreterm n wordMap delimiterToken) delimiterToken
  U.Proj s m -> wrapPreterm ([Proj'] ++ selectorToToken s ++ splitPreterm m wordMap delimiterToken) delimiterToken
  U.Disj a b -> wrapPreterm ([Disj'] ++ splitPreterm a wordMap delimiterToken ++ splitPreterm b wordMap delimiterToken) delimiterToken
  U.Iota s m -> wrapPreterm ([Iota'] ++ selectorToToken s ++ splitPreterm m wordMap delimiterToken) delimiterToken
  U.Unpack p h m n -> wrapPreterm ([Unpack'] ++ splitPreterm p wordMap delimiterToken ++ splitPreterm h wordMap delimiterToken ++ splitPreterm m wordMap delimiterToken ++ splitPreterm n wordMap delimiterToken) delimiterToken
  U.Bot -> wrapPreterm [Bot'] delimiterToken
  U.Unit -> wrapPreterm [Unit'] delimiterToken
  U.Top -> wrapPreterm [Top'] delimiterToken
  U.Entity -> wrapPreterm [Entity'] delimiterToken
  U.Nat -> wrapPreterm [Nat'] delimiterToken
  U.Zero -> wrapPreterm [Zero'] delimiterToken
  U.Succ m -> wrapPreterm ([Succ'] ++ splitPreterm m wordMap delimiterToken) delimiterToken
  U.Natrec p n e f -> wrapPreterm ([Natrec'] ++ splitPreterm p wordMap delimiterToken ++ splitPreterm n wordMap delimiterToken ++ splitPreterm e wordMap delimiterToken ++ splitPreterm f wordMap delimiterToken) delimiterToken
  U.Eq a m n -> wrapPreterm ([Eq'] ++ splitPreterm a wordMap delimiterToken ++ splitPreterm m wordMap delimiterToken ++ splitPreterm n wordMap delimiterToken) delimiterToken
  U.Refl a m -> wrapPreterm ([Refl'] ++ splitPreterm a wordMap delimiterToken ++ splitPreterm m wordMap delimiterToken) delimiterToken
  U.Idpeel p e r -> wrapPreterm ([Idpeel'] ++ splitPreterm p wordMap delimiterToken ++ splitPreterm e wordMap delimiterToken ++ splitPreterm r wordMap delimiterToken) delimiterToken

splitPreterms:: [U.Preterm] -> WordMap -> DelimiterToken -> [Token]
splitPreterms preterms wordMap delimiterToken = concatMap (\preterm -> splitPreterm preterm wordMap delimiterToken) preterms

wrapPair :: [Token] -> DelimiterToken -> [Token]
wrapPair xs Paren = [LPAREN] ++ xs ++ [RPAREN]
wrapPair xs Sep = xs ++ [SEP]
wrapPair xs Eo = xs ++ [EOPair]
wrapPair xs Unused = xs

splitSignature :: U.Signature -> WordMap -> DelimiterToken -> [Token]
splitSignature signature wordMap delimiterToken = concatMap (\(name, preterm) -> wrapPair (textToToken name wordMap ++ [COMMA] ++ splitPreterm preterm wordMap delimiterToken) delimiterToken) signature

wrapSignature :: [Token] -> DelimiterToken -> [Token]
wrapSignature xs Paren = [LPAREN] ++ xs ++ [RPAREN]
wrapSignature xs Sep = xs ++ [SEP]
wrapSignature xs Eo = xs ++ [EOSig]
wrapSignature xs Unused = xs

wrapContext :: [Token] -> DelimiterToken -> [Token]
wrapContext xs Paren = [LPAREN] ++ xs ++ [RPAREN]
wrapContext xs Sep = xs ++ [SEP]
wrapContext xs Eo = xs ++ [EOCon]
wrapContext xs Unused = xs

wrapTerm ::[Token] -> DelimiterToken -> [Token]
wrapTerm xs Paren = [LPAREN] ++ xs ++ [RPAREN]
wrapTerm xs Sep = xs ++ [SEP]
wrapTerm xs Eo = xs ++ [EOTerm]
wrapTerm xs Unused = xs

wrapTyp :: [Token] -> DelimiterToken -> [Token]
wrapTyp xs Paren = [LPAREN] ++ xs ++ [RPAREN]
wrapTyp xs Sep = xs ++ [SEP]
wrapTyp xs Eo = xs ++ [EOTyp]
wrapTyp xs Unused = xs

splitJudgment :: U.Judgment -> WordMap -> DelimiterToken -> [Token]
splitJudgment judgment wordMap delimiterToken =
  if isIncludeTerm
    then  wrapSignature (splitSignature (U.signtr judgment) wordMap delimiterToken) delimiterToken ++
          wrapContext (splitPreterms (U.contxt judgment) wordMap delimiterToken) delimiterToken ++
          wrapTerm (splitPreterm (U.trm judgment) wordMap delimiterToken) delimiterToken ++
          wrapTyp (splitPreterm (U.typ judgment) wordMap delimiterToken) delimiterToken
    else  wrapSignature (splitSignature (U.signtr judgment) wordMap delimiterToken) delimiterToken ++
          wrapContext (splitPreterms (U.contxt judgment) wordMap delimiterToken) delimiterToken ++
          wrapTyp (splitPreterm (U.typ judgment) wordMap delimiterToken) delimiterToken

-- | DTTruleからRuleLabelへの変換関数
-- lightblueのBR.dttruleToRuleLabelをエクスポート
dttruleToRuleLabel :: QT.DTTrule -> Maybe BR.RuleLabel
dttruleToRuleLabel = BR.dttruleToRuleLabel