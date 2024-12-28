{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module SplitJudgment
    ( loadActionsFromBinary
    , getWordsFromJudgment
    , getFrequentWords
    , Token(..)
    , splitJudgment
    , countRule
    , copyData
    , replaceData
    ) where

import qualified DTS.QueryTypes as QT
import qualified DTS.DTTdeBruijn as U
import qualified Data.ByteString as B --bytestring
import qualified Data.Text.Lazy as T  --text
import Data.Store (decode)
import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Data.Ord (Down(..))
import qualified Data.Set as Set
import Data.Function (fix)
import System.Random (randomRIO)

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
getFrequentWords frequentWords = take 31 $ map fst $ sortOn (Down . snd) $ Map.toList wordFreqMap
  where
    wordFreqMap :: Map.Map T.Text Int
    wordFreqMap = foldr (\word acc -> Map.insertWith (+) word 1 acc) Map.empty frequentWords

data Token =  FST | SND | COMMA | EOPair | EOPre | EOSig | EOCon | EOTerm | EOTyp | LPAREN | RPAREN | SEP
            | Word1 | Word2 | Word3 | Word4 | Word5 | Word6 | Word7 | Word8 | Word9 | Word10 | Word11 | Word12 | Word13 | Word14 | Word15 | Word16 | Word17 | Word18 | Word19 | Word20 | Word21 | Word22 | Word23 | Word24 | Word25 | Word26 | Word27 | Word28 | Word29 | Word30 | Word31 | UNKNOWN
            | Var'0 | Var'1 | Var'2 | Var'3 | Var'4 | Var'5 | Var'6 | Var'unknown
            | Type' | Kind' | Pi' | Lam' | App' | Not' | Sigma' | Pair' | Proj' | Disj' | Iota' | Unpack' | Bot' | Unit' | Top' | Entity' | Nat' | Zero' | Succ' | Natrec' | Eq' | Refl' | Idpeel'
  deriving (Enum, Show, Bounded, Eq, Ord)

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

wrapPreterm :: [Token] -> Bool -> Bool -> [Token]
wrapPreterm xs isParen isSep =
  if isParen then [LPAREN] ++ xs ++ [RPAREN]
  else if isSep then xs ++ [SEP] else xs ++ [EOPre]

splitPreterm :: U.Preterm -> [T.Text] -> Bool -> Bool -> [Token]
splitPreterm preterm frequentWords isParen isSep = case preterm of
  U.Var i -> wrapPreterm (varToToken i) isParen isSep
  U.Con c -> wrapPreterm (textToToken c frequentWords) isParen isSep
  U.Type -> wrapPreterm [Type'] isParen isSep
  U.Kind -> wrapPreterm [Kind'] isParen isSep
  U.Pi a b -> wrapPreterm ([Pi'] ++ splitPreterm a frequentWords isParen isSep ++ splitPreterm b frequentWords isParen isSep) isParen isSep
  U.Lam m -> wrapPreterm ([Lam'] ++ splitPreterm m frequentWords isParen isSep) isParen isSep
  U.App m n -> wrapPreterm ([App'] ++ splitPreterm m frequentWords isParen isSep ++ splitPreterm n frequentWords isParen isSep) isParen isSep
  U.Not m -> wrapPreterm ([Not'] ++ splitPreterm m frequentWords isParen isSep) isParen isSep
  U.Sigma a b -> wrapPreterm ([Sigma'] ++ splitPreterm a frequentWords isParen isSep ++ splitPreterm b frequentWords isParen isSep) isParen isSep
  U.Pair m n -> wrapPreterm ([Pair'] ++ splitPreterm m frequentWords isParen isSep ++ splitPreterm n frequentWords isParen isSep) isParen isSep
  U.Proj s m -> wrapPreterm ([Proj'] ++ selectorToToken s ++ splitPreterm m frequentWords isParen isSep) isParen isSep
  U.Disj a b -> wrapPreterm ([Disj'] ++ splitPreterm a frequentWords isParen isSep ++ splitPreterm b frequentWords isParen isSep) isParen isSep
  U.Iota s m -> wrapPreterm ([Iota'] ++ selectorToToken s ++ splitPreterm m frequentWords isParen isSep) isParen isSep
  U.Unpack p h m n -> wrapPreterm ([Unpack'] ++ splitPreterm p frequentWords isParen isSep ++ splitPreterm h frequentWords isParen isSep ++ splitPreterm m frequentWords isParen isSep ++ splitPreterm n frequentWords isParen isSep) isParen isSep
  U.Bot -> wrapPreterm [Bot'] isParen isSep
  U.Unit -> wrapPreterm [Unit'] isParen isSep
  U.Top -> wrapPreterm [Top'] isParen isSep
  U.Entity -> wrapPreterm [Entity'] isParen isSep
  U.Nat -> wrapPreterm [Nat'] isParen isSep
  U.Zero -> wrapPreterm [Zero'] isParen isSep
  U.Succ m -> wrapPreterm ([Succ'] ++ splitPreterm m frequentWords isParen isSep) isParen isSep
  U.Natrec n e f -> wrapPreterm ([Natrec'] ++ splitPreterm n frequentWords isParen isSep ++ splitPreterm e frequentWords isParen isSep ++ splitPreterm f frequentWords isParen isSep) isParen isSep
  U.Eq a m n -> wrapPreterm ([Eq'] ++ splitPreterm a frequentWords isParen isSep ++ splitPreterm m frequentWords isParen isSep ++ splitPreterm n frequentWords isParen isSep) isParen isSep
  U.Refl a m -> wrapPreterm ([Refl'] ++ splitPreterm a frequentWords isParen isSep ++ splitPreterm m frequentWords isParen isSep) isParen isSep
  U.Idpeel m n -> wrapPreterm ([Idpeel'] ++ splitPreterm m frequentWords isParen isSep ++ splitPreterm n frequentWords isParen isSep) isParen isSep

splitPreterms:: [U.Preterm] -> [T.Text] -> Bool -> Bool -> [Token]
splitPreterms preterms frequentWords isParen isSep = concatMap (\preterm -> splitPreterm preterm frequentWords isParen isSep) preterms

wrapPair :: [Token] -> Bool -> Bool -> [Token]
wrapPair xs isParen isSep =
  if isParen then [LPAREN] ++ xs ++ [RPAREN]
  else if isSep then xs ++ [SEP] else xs ++ [EOPair]

splitSignature :: U.Signature -> [T.Text] -> Bool -> Bool -> [Token]
splitSignature signature frequentWords isParen isSep = concatMap (\(name, preterm) -> wrapPair (textToToken name frequentWords ++ [COMMA] ++ splitPreterm preterm frequentWords isParen isSep) isParen isSep) signature

wrapSignature :: [Token] -> Bool -> Bool -> [Token]
wrapSignature xs isParen isSep =
  if isParen then [LPAREN] ++ xs ++ [RPAREN]
  else if isSep then xs ++ [SEP] else xs ++ [EOSig]

wrapContext :: [Token] -> Bool -> Bool -> [Token]
wrapContext xs isParen isSep =
  if isParen then [LPAREN] ++ xs ++ [RPAREN]
  else if isSep then xs ++ [SEP] else xs ++ [EOCon]

wrapTerm ::[Token] -> Bool -> Bool -> [Token]
wrapTerm xs isParen isSep =
  if isParen then [LPAREN] ++ xs ++ [RPAREN]
  else if isSep then xs ++ [SEP] else xs ++ [EOTerm]

wrapTyp :: [Token] -> Bool -> Bool -> [Token]
wrapTyp xs isParen isSep =
  if isParen then [LPAREN] ++ xs ++ [RPAREN]
  else if isSep then xs else xs ++ [EOTyp]

splitJudgment :: U.Judgment -> [T.Text] -> Bool -> Bool -> [Token]
splitJudgment judgment frequentWords isParen isSep =
  wrapSignature (splitSignature (U.signtr judgment) frequentWords isParen isSep) isParen isSep ++
  wrapContext (splitPreterms (U.contxt judgment) frequentWords isParen isSep) isParen isSep ++
  wrapTerm (splitPreterm (U.trm judgment) frequentWords isParen isSep) isParen isSep ++
  wrapTyp (splitPreterm (U.typ judgment) frequentWords isParen isSep) isParen isSep

countRule :: [QT.DTTrule] -> [(QT.DTTrule, Int)]
countRule rules = sortOn (Down . snd) $ Map.toList ruleFreqMap
  where
    ruleFreqMap :: Map.Map QT.DTTrule Int
    ruleFreqMap = foldr (\word acc -> Map.insertWith (+) word 1 acc) Map.empty rules

splitByLabel :: [([Token], QT.DTTrule)] -> IO [(QT.DTTrule, [([Token], QT.DTTrule)])]
splitByLabel dataset = do
  flip fix (0, dataset, []) $ \loop (i, datalist, splittedData) -> do
    if datalist == [] then return splittedData
    else do
      let (tokens, rule) = head datalist
          data' = (tokens, rule)
          rest = tail datalist
          splittedData' = Map.toList $ Map.insertWith (++) rule [data'] (Map.fromList splittedData)
      loop (i + 1, rest, splittedData')

-- 最も数が多いラベルと同数、各ラベルでデータを複製する
copyData :: [([Token], QT.DTTrule)] -> IO [([Token], QT.DTTrule)]
copyData dataset = do
  let countedRules = countRule $ map (\(_, rule) -> rule) dataset
      maxCount = snd $ head countedRules
  splitedbyLabel <- splitByLabel dataset
  flip fix (0, splitedbyLabel, []) $ \loop (i, datalist, copiedData) -> do
    if datalist == [] then return copiedData
    else do
      let (_, datas) = head datalist
          rest = tail datalist
          replicateCount = maxCount `div` (length datas)
          modCount = maxCount `mod` length datas
          copiedData' = copiedData ++ (concat $ replicate replicateCount datas) ++ take modCount datas
      print $ "length copiedData' " ++ show (length copiedData')
      loop (i + 1, rest, copiedData')

replaceData :: [([Token], QT.DTTrule)] -> IO [([Token], QT.DTTrule)]
replaceData dataset = do
  let countedRules = countRule $ map (\(_, rule) -> rule) dataset
      maxCount = snd $ head countedRules
  splitedbyLabel <- splitByLabel dataset
  flip fix (0, splitedbyLabel, []) $ \loop (i, datalist, generatedDataList) -> do
    if datalist == [] then return generatedDataList
    else do
      let (_, datas) = head datalist
          rest = tail datalist
          generateCount = maxCount - length datas
          replicateCount = generateCount `div` (length datas)
          modCount = generateCount `mod` length datas
          copiedData = (concat $ replicate replicateCount datas) ++ take modCount datas
      newDatas <- mapM (\oneData -> do
        replacedData <- replaceToken $ fst oneData
        return (replacedData, snd oneData)) copiedData
      loop (i + 1, rest, generatedDataList ++ datas ++ newDatas)

replaceToken :: [Token] -> IO [Token]
replaceToken tokenList = do
  let wordTokenList = [Word1, Word2, Word3, Word4, Word5, Word6, Word7, Word8, Word9, Word10, Word11, Word12, Word13, Word14, Word15, Word16, Word17, Word18, Word19, Word20, Word21, Word22, Word23, Word24, Word25, Word26, Word27, Word28, Word29, Word30, Word31]
      varTokenList = [Var'0, Var'1, Var'2, Var'3, Var'4, Var'5, Var'6]
  randomWordIndex <- randomRIO (0, 30)
  randomVarIndex <- randomRIO (0, 6)
  let replacedList = map (\token -> case token of
        Word1 -> wordTokenList !! randomWordIndex
        Word2 -> wordTokenList !! ((randomWordIndex + 1) `mod` 31)
        Word3 -> wordTokenList !! ((randomWordIndex + 2) `mod` 31)
        Word4 -> wordTokenList !! ((randomWordIndex + 3) `mod` 31)
        Word5 -> wordTokenList !! ((randomWordIndex + 4) `mod` 31)
        Word6 -> wordTokenList !! ((randomWordIndex + 5) `mod` 31)
        Word7 -> wordTokenList !! ((randomWordIndex + 6) `mod` 31)
        Word8 -> wordTokenList !! ((randomWordIndex + 7) `mod` 31)
        Word9 -> wordTokenList !! ((randomWordIndex + 8) `mod` 31)
        Word10 -> wordTokenList !! ((randomWordIndex + 9) `mod` 31)
        Word11 -> wordTokenList !! ((randomWordIndex + 10) `mod` 31)
        Word12 -> wordTokenList !! ((randomWordIndex + 11) `mod` 31)
        Word13 -> wordTokenList !! ((randomWordIndex + 12) `mod` 31)
        Word14 -> wordTokenList !! ((randomWordIndex + 13) `mod` 31)
        Word15 -> wordTokenList !! ((randomWordIndex + 14) `mod` 31)
        Word16 -> wordTokenList !! ((randomWordIndex + 15) `mod` 31)
        Word17 -> wordTokenList !! ((randomWordIndex + 16) `mod` 31)
        Word18 -> wordTokenList !! ((randomWordIndex + 17) `mod` 31)
        Word19 -> wordTokenList !! ((randomWordIndex + 18) `mod` 31)
        Word20 -> wordTokenList !! ((randomWordIndex + 19) `mod` 31)
        Word21 -> wordTokenList !! ((randomWordIndex + 20) `mod` 31)
        Word22 -> wordTokenList !! ((randomWordIndex + 21) `mod` 31)
        Word23 -> wordTokenList !! ((randomWordIndex + 22) `mod` 31)
        Word24 -> wordTokenList !! ((randomWordIndex + 23) `mod` 31)
        Word25 -> wordTokenList !! ((randomWordIndex + 24) `mod` 31)
        Word26 -> wordTokenList !! ((randomWordIndex + 25) `mod` 31)
        Word27 -> wordTokenList !! ((randomWordIndex + 26) `mod` 31)
        Word28 -> wordTokenList !! ((randomWordIndex + 27) `mod` 31)
        Word29 -> wordTokenList !! ((randomWordIndex + 28) `mod` 31)
        Word30 -> wordTokenList !! ((randomWordIndex + 29) `mod` 31)
        Word31 -> wordTokenList !! ((randomWordIndex + 30) `mod` 31)
        Var'0 -> varTokenList !! randomVarIndex
        Var'1 -> varTokenList !! ((randomVarIndex + 1) `mod` 7)
        Var'2 -> varTokenList !! ((randomVarIndex + 2) `mod` 7)
        Var'3 -> varTokenList !! ((randomVarIndex + 3) `mod` 7)
        Var'4 -> varTokenList !! ((randomVarIndex + 4) `mod` 7)
        Var'5 -> varTokenList !! ((randomVarIndex + 5) `mod` 7)
        Var'6 -> varTokenList !! ((randomVarIndex + 6) `mod` 7)
        _ -> token) tokenList
  return replacedList