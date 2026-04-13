{-# LANGUAGE MultiParamTypeClasses #-}

module Training.Shared
  ( DatasetType (..),
    backwardRules,
    formationRules,
    countRule,
    splitByLabel,
    smoothData,
    parseDatasetType,
    loadDataset,
  ) where

import Control.Monad (forM)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import System.Random.Shuffle (shuffleM)

import qualified DTS.DTTdeBruijn as U
import qualified DTS.Prover.Wani.BackwardRules as BR
import qualified DTS.QueryTypes as QT
import SplitJudgment (Token(..), loadActionsFromBinary)

-- | データセットの種類を表すデータ型
-- JSeM か TPTP を選ぶ（TPTPのフォルダ指定文字列はログ用に保持）
data DatasetType = JSeM | TPTP String
  deriving (Show, Read, Eq)

backwardRules :: [QT.DTTrule]
backwardRules = [QT.PiF, QT.SigmaF, QT.IqF, QT.Var, QT.Con, QT.PiI, QT.SigmaI, QT.PiE, QT.TopI, QT.DisjI, QT.DisjE, QT.DisjF, QT.DNE, QT.EFQ]

formationRules :: [QT.DTTrule]
formationRules = [QT.TypeF, QT.PiF, QT.SigmaF, QT.DisjF, QT.BotF, QT.TopF, QT.EnumF, QT.IqF, QT.NatF]

countRule :: [BR.RuleLabel] -> [(BR.RuleLabel, Int)]
countRule rules = List.sortOn (negate . snd) $ Map.toList ruleFreqMap
  where
    ruleFreqMap :: Map.Map BR.RuleLabel Int
    ruleFreqMap = foldr (\word acc -> Map.insertWith (+) word 1 acc) Map.empty rules

splitByLabel :: [([Token], BR.RuleLabel)] -> IO [(BR.RuleLabel, [([Token], BR.RuleLabel)])]
splitByLabel dataset = pure $ splitByLabel' dataset Map.empty
  where
    splitByLabel' [] acc = Map.toList acc
    splitByLabel' ((tokens', rule) : xs) acc =
      let acc' = Map.insertWith (++) rule [(tokens', rule)] acc
       in splitByLabel' xs acc'

smoothData :: [(BR.RuleLabel, [([Token], BR.RuleLabel)])] -> Maybe Int -> IO ([([Token], BR.RuleLabel)], [([Token], BR.RuleLabel)], [([Token], BR.RuleLabel)])
smoothData splittedData threshold = smoothData' splittedData [] [] []
  where
    smoothData' [] trainDataAcc validDataAcc testDataAcc = pure (trainDataAcc, validDataAcc, testDataAcc)
    smoothData' ((_, dataList) : remainingData) trainDataAcc validDataAcc testDataAcc = do
      shuffledData <- shuffleM dataList
      let limitedData = case threshold of
            Nothing -> shuffledData
            Just threshold' -> take threshold' shuffledData
          (trainData, restData) = splitAt (length limitedData * 8 `div` 10) limitedData
          (validData, testData) = splitAt (length restData * 5 `div` 10) restData
      smoothData' remainingData (trainDataAcc ++ trainData) (validDataAcc ++ validData) (testDataAcc ++ testData)

parseDatasetType :: String -> DatasetType
parseDatasetType "jsem" = JSeM
parseDatasetType "tptp" = TPTP "all"
parseDatasetType s
  | "tptp:" `List.isPrefixOf` s = TPTP (drop 5 s)
  | otherwise = error $ "Unknown dataset type: " ++ s ++ ". Use 'jsem' or 'tptp'"

loadDataset :: DatasetType -> IO ([(U.Judgment, QT.DTTrule)], [FilePath])
loadDataset JSeM = do
  jsemFiles <- listDirectory "data/JSeM/"
  datasets <- mapM (\file -> loadActionsFromBinary ("data/JSeM/" </> file)) jsemFiles
  pure (concat datasets, [])
loadDataset (TPTP folderName) = do
  let tptpBasePath = "tptp-judgment-rule-pairs"
  baseExists <- doesDirectoryExist tptpBasePath
  if not baseExists
    then error $ "TPTP base folder not found: " ++ tptpBasePath
    else do
      binFiles <- List.sort <$> findBinFiles tptpBasePath
      if null binFiles
        then error $ "No .bin files found under: " ++ tptpBasePath
        else do
          putStrLn $ "Loading " ++ show (length binFiles) ++ " .bin files from " ++ tptpBasePath ++ " (requested folder: " ++ folderName ++ ")"
          mapM_ (putStrLn . ("  using: " ++)) binFiles
          datasets <- mapM loadActionsFromBinary binFiles
          pure (concat datasets, binFiles)
  where
    findBinFiles :: FilePath -> IO [FilePath]
    findBinFiles dir = do
      entries <- listDirectory dir
      paths <- forM entries $ \entry -> do
        let path = dir </> entry
        isDir <- doesDirectoryExist path
        if isDir
          then findBinFiles path
          else pure [path | takeExtension entry == ".bin"]
      pure (concat paths)
