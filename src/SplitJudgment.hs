{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module SplitJudgment
    ( loadActionsFromBinary
    , getWordsFromJudgment
    , getFrequentWords
    , Token(..)
    , splitJudgment
    , DelimiterToken(..)
    ) where

import qualified DTS.QueryTypes as QT
import qualified DTS.DTTdeBruijn as U
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

getWordsFromPreterm :: U.Preterm -> [T.Text]
getWordsFromPreterm preterm = case preterm of
  U.Con c  -> [c]
  U.Pi a b -> getWordsFromPreterm a ++ getWordsFromPreterm b
  U.Lam m  -> getWordsFromPreterm m
  U.App m n -> getWordsFromPreterm m ++ getWordsFromPreterm n
  U.Not m  -> getWordsFromPreterm m
  U.Sigma a b  -> getWordsFromPreterm a ++ getWordsFromPreterm b
  U.Pair m n   -> getWordsFromPreterm m ++ getWordsFromPreterm n
  U.Proj _ m   -> getWordsFromPreterm m
  U.Disj a b   -> getWordsFromPreterm a ++ getWordsFromPreterm b
  U.Iota _ m   -> getWordsFromPreterm m
  U.Unpack p h m n -> getWordsFromPreterm p ++ getWordsFromPreterm h ++ getWordsFromPreterm m ++ getWordsFromPreterm n
  U.Succ n     -> getWordsFromPreterm n
  U.Natrec n e f -> getWordsFromPreterm n ++ getWordsFromPreterm e ++ getWordsFromPreterm f
  U.Eq a m n   -> getWordsFromPreterm a ++ getWordsFromPreterm m ++ getWordsFromPreterm n
  U.Refl a m   -> getWordsFromPreterm a ++ getWordsFromPreterm m
  U.Idpeel m n -> getWordsFromPreterm m ++ getWordsFromPreterm n
  _ -> []

getWordsFromPreterms :: [U.Preterm] -> [T.Text]
getWordsFromPreterms preterms = concatMap (\preterm -> getWordsFromPreterm preterm) preterms

getWordsFromSignature :: U.Signature -> [T.Text]
getWordsFromSignature signature = concatMap (\(name, preterm) -> [name] ++ getWordsFromPreterm preterm) signature

allowDuplicateWords :: Bool
allowDuplicateWords = True

getWordsFromJudgment :: U.Judgment -> [T.Text]
getWordsFromJudgment judgment =
  if allowDuplicateWords then wordList
  else Set.toList . Set.fromList $ wordList
  where
    wordList =
      getWordsFromSignature (U.signtr judgment) ++
      getWordsFromPreterms (U.contxt judgment) ++
      getWordsFromPreterm (U.trm judgment) ++
      getWordsFromPreterm (U.typ judgment)

getFrequentWords :: [T.Text] -> [T.Text]
getFrequentWords frequentWords = take 31 $ map fst $ List.sortOn (Down . snd) $ Map.toList wordFreqMap
  where
    wordFreqMap :: Map.Map T.Text Int
    wordFreqMap = foldr (\word acc -> Map.insertWith (+) word 1 acc) Map.empty frequentWords

data Token =  FST | SND | COMMA | EOPair | EOPre | EOSig | EOCon | EOTerm | EOTyp | LPAREN | RPAREN | SEP
            | Word1 | Word2 | Word3 | Word4 | Word5 | Word6 | Word7 | Word8 | Word9 | Word10 | Word11 | Word12 | Word13 | Word14 | Word15 | Word16 | Word17 | Word18 | Word19 | Word20 | Word21 | Word22 | Word23 | Word24 | Word25 | Word26 | Word27 | Word28 | Word29 | Word30 | Word31 | UNKNOWN
            | Var'0 | Var'1 | Var'2 | Var'3 | Var'4 | Var'5 | Var'6 | Var'unknown
            | Type' | Kind' | Pi' | Lam' | App' | Not' | Sigma' | Pair' | Proj' | Disj' | Iota' | Unpack' | Bot' | Unit' | Top' | Entity' | Nat' | Zero' | Succ' | Natrec' | Eq' | Refl' | Idpeel'
  deriving (Enum, Show, Bounded, Eq, Ord)

data DelimiterToken = Paren | Sep | Eo | Unused deriving (Show, Eq, Enum, Bounded, Ord, Generic, Read)

textToToken :: T.Text -> [T.Text] -> [Token]
textToToken text frequentWords =
  case () of
    _ | length frequentWords > 0 && text == frequentWords !! 0 -> [Word1]
      | length frequentWords > 1 && text == frequentWords !! 1 -> [Word2]
      | length frequentWords > 2 && text == frequentWords !! 2 -> [Word3]
      | length frequentWords > 3 && text == frequentWords !! 3 -> [Word4]
      | length frequentWords > 4 && text == frequentWords !! 4 -> [Word5]
      | length frequentWords > 5 && text == frequentWords !! 5 -> [Word6]
      | length frequentWords > 6 && text == frequentWords !! 6 -> [Word7]
      | length frequentWords > 7 && text == frequentWords !! 7 -> [Word8]
      | length frequentWords > 8 && text == frequentWords !! 8 -> [Word9]
      | length frequentWords > 9 && text == frequentWords !! 9 -> [Word10]
      | length frequentWords > 10 && text == frequentWords !! 10 -> [Word11]
      | length frequentWords > 11 && text == frequentWords !! 11 -> [Word12]
      | length frequentWords > 12 && text == frequentWords !! 12 -> [Word13]
      | length frequentWords > 13 && text == frequentWords !! 13 -> [Word14]
      | length frequentWords > 14 && text == frequentWords !! 14 -> [Word15]
      | length frequentWords > 15 && text == frequentWords !! 15 -> [Word16]
      | length frequentWords > 16 && text == frequentWords !! 16 -> [Word17]
      | length frequentWords > 17 && text == frequentWords !! 17 -> [Word18]
      | length frequentWords > 18 && text == frequentWords !! 18 -> [Word19]
      | length frequentWords > 19 && text == frequentWords !! 19 -> [Word20]
      | length frequentWords > 20 && text == frequentWords !! 20 -> [Word21]
      | length frequentWords > 21 && text == frequentWords !! 21 -> [Word22]
      | length frequentWords > 22 && text == frequentWords !! 22 -> [Word23]
      | length frequentWords > 23 && text == frequentWords !! 23 -> [Word24]
      | length frequentWords > 24 && text == frequentWords !! 24 -> [Word25]
      | length frequentWords > 25 && text == frequentWords !! 25 -> [Word26]
      | length frequentWords > 26 && text == frequentWords !! 26 -> [Word27]
      | length frequentWords > 27 && text == frequentWords !! 27 -> [Word28]
      | length frequentWords > 28 && text == frequentWords !! 28 -> [Word29]
      | length frequentWords > 29 && text == frequentWords !! 29 -> [Word30]
      | length frequentWords > 30 && text == frequentWords !! 30 -> [Word31]
      | otherwise -> [UNKNOWN]

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

wrapPreterm :: [Token] -> DelimiterToken -> [Token]
wrapPreterm xs Paren = [LPAREN] ++ xs ++ [RPAREN]
wrapPreterm xs Sep = xs ++ [SEP]
wrapPreterm xs Eo = xs ++ [EOPre]
wrapPreterm xs Unused = xs

splitPreterm :: U.Preterm -> [T.Text] -> DelimiterToken -> [Token]
splitPreterm preterm frequentWords delimiterToken = case preterm of
  U.Var i -> wrapPreterm (varToToken i) delimiterToken
  U.Con c -> wrapPreterm (textToToken c frequentWords) delimiterToken
  U.Type -> wrapPreterm [Type'] delimiterToken
  U.Kind -> wrapPreterm [Kind'] delimiterToken
  U.Pi a b -> wrapPreterm ([Pi'] ++ splitPreterm a frequentWords delimiterToken ++ splitPreterm b frequentWords delimiterToken) delimiterToken
  U.Lam m -> wrapPreterm ([Lam'] ++ splitPreterm m frequentWords delimiterToken) delimiterToken
  U.App m n -> wrapPreterm ([App'] ++ splitPreterm m frequentWords delimiterToken ++ splitPreterm n frequentWords delimiterToken) delimiterToken
  U.Not m -> wrapPreterm ([Not'] ++ splitPreterm m frequentWords delimiterToken) delimiterToken
  U.Sigma a b -> wrapPreterm ([Sigma'] ++ splitPreterm a frequentWords delimiterToken ++ splitPreterm b frequentWords delimiterToken) delimiterToken
  U.Pair m n -> wrapPreterm ([Pair'] ++ splitPreterm m frequentWords delimiterToken ++ splitPreterm n frequentWords delimiterToken) delimiterToken
  U.Proj s m -> wrapPreterm ([Proj'] ++ selectorToToken s ++ splitPreterm m frequentWords delimiterToken) delimiterToken
  U.Disj a b -> wrapPreterm ([Disj'] ++ splitPreterm a frequentWords delimiterToken ++ splitPreterm b frequentWords delimiterToken) delimiterToken
  U.Iota s m -> wrapPreterm ([Iota'] ++ selectorToToken s ++ splitPreterm m frequentWords delimiterToken) delimiterToken
  U.Unpack p h m n -> wrapPreterm ([Unpack'] ++ splitPreterm p frequentWords delimiterToken ++ splitPreterm h frequentWords delimiterToken ++ splitPreterm m frequentWords delimiterToken ++ splitPreterm n frequentWords delimiterToken) delimiterToken
  U.Bot -> wrapPreterm [Bot'] delimiterToken
  U.Unit -> wrapPreterm [Unit'] delimiterToken
  U.Top -> wrapPreterm [Top'] delimiterToken
  U.Entity -> wrapPreterm [Entity'] delimiterToken
  U.Nat -> wrapPreterm [Nat'] delimiterToken
  U.Zero -> wrapPreterm [Zero'] delimiterToken
  U.Succ m -> wrapPreterm ([Succ'] ++ splitPreterm m frequentWords delimiterToken) delimiterToken
  U.Natrec n e f -> wrapPreterm ([Natrec'] ++ splitPreterm n frequentWords delimiterToken ++ splitPreterm e frequentWords delimiterToken ++ splitPreterm f frequentWords delimiterToken) delimiterToken
  U.Eq a m n -> wrapPreterm ([Eq'] ++ splitPreterm a frequentWords delimiterToken ++ splitPreterm m frequentWords delimiterToken ++ splitPreterm n frequentWords delimiterToken) delimiterToken
  U.Refl a m -> wrapPreterm ([Refl'] ++ splitPreterm a frequentWords delimiterToken ++ splitPreterm m frequentWords delimiterToken) delimiterToken
  U.Idpeel m n -> wrapPreterm ([Idpeel'] ++ splitPreterm m frequentWords delimiterToken ++ splitPreterm n frequentWords delimiterToken) delimiterToken

splitPreterms:: [U.Preterm] -> [T.Text] -> DelimiterToken -> [Token]
splitPreterms preterms frequentWords delimiterToken = concatMap (\preterm -> splitPreterm preterm frequentWords delimiterToken) preterms

wrapPair :: [Token] -> DelimiterToken -> [Token]
wrapPair xs Paren = [LPAREN] ++ xs ++ [RPAREN]
wrapPair xs Sep = xs ++ [SEP]
wrapPair xs Eo = xs ++ [EOPair]
wrapPair xs Unused = xs

splitSignature :: U.Signature -> [T.Text] -> DelimiterToken -> [Token]
splitSignature signature frequentWords delimiterToken = concatMap (\(name, preterm) -> wrapPair (textToToken name frequentWords ++ [COMMA] ++ splitPreterm preterm frequentWords delimiterToken) delimiterToken) signature

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

splitJudgment :: U.Judgment -> [T.Text] -> DelimiterToken -> [Token]
splitJudgment judgment frequentWords delimiterToken =
  wrapSignature (splitSignature (U.signtr judgment) frequentWords delimiterToken) delimiterToken ++
  wrapContext (splitPreterms (U.contxt judgment) frequentWords delimiterToken) delimiterToken ++
  wrapTerm (splitPreterm (U.trm judgment) frequentWords delimiterToken) delimiterToken ++
  wrapTyp (splitPreterm (U.typ judgment) frequentWords delimiterToken) delimiterToken