{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (forM)
import qualified Data.ByteString as B
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import Data.Store (decode)
import Data.Maybe (mapMaybe)
import Data.List (sort, isPrefixOf)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Environment (getArgs)
import System.Directory (listDirectory, createDirectoryIfMissing, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeBaseName, takeExtension)

import qualified DTS.DTTdeBruijn as DTT
import qualified DTS.QueryTypes as QT
import qualified DTS.Prover.Wani.BackwardRules as BR
import Torch.Device (Device(..), DeviceType(..))

import ML.Exp.Chart (drawConfusionMatrix)
import ML.Exp.Classification (showClassificationReport)

import Forward (HypParams(..), Params)
import SplitJudgment (Token(..), DelimiterToken(..), splitJudgment, dttruleToRuleLabel, buildWordMap, WordMap)
import Evaluate (loadModelAndFrequentWords, predictOnTestData, EvaluationResult(..), evaluateModel, saveEvaluationReport)

-- | 抽出されたバイナリファイルをロードする
loadExtractedData :: FilePath -> IO [(DTT.Judgment, QT.DTTrule)]
loadExtractedData filepath = do
  binary <- B.readFile filepath
  case decode binary of
    Left err -> error $ "Could not parse file " ++ filepath ++ ": " ++ show err
    Right actions -> return actions

-- | DTTruleからRuleLabelへの変換（フィルタリング付き）
convertToRuleLabel :: (DTT.Judgment, QT.DTTrule) -> Maybe (DTT.Judgment, BR.RuleLabel)
convertToRuleLabel (judgment, dttRule) = 
  case dttruleToRuleLabel dttRule of
    Just ruleLabel -> Just (judgment, ruleLabel)
    Nothing -> Nothing

-- | デフォルトのハイパーパラメータ（NeuralWaniBuilderと同じ設定）
defaultHyperParams :: HypParams
defaultHyperParams = HypParams
  { dev = Device CPU 0
  , bi_directional = False
  , emb_dim = 256
  , has_bias = False
  , proj_size = Nothing
  , vocab_size = length (enumFrom minBound :: [Token])
  , num_layers = 1
  , hidden_size = 256
  , num_rules = length (enumFrom minBound :: [BR.RuleLabel])
  }

-- | デフォルトのモデルパス
defaultModelPath :: FilePath
defaultModelPath = "trainedDataBackwardWithoutF/typeUnused_biFalse_s32_lr5.0e-4_i256_h256_layer1/2025-12-15_12-14-54/seq-class.model"

-- | デフォルトのfrequentWordsパス
defaultFrequentWordsPath :: FilePath
defaultFrequentWordsPath = "trainedDataBackwardWithoutF/typeUnused_biFalse_s32_lr5.0e-4_i256_h256_layer1/2025-12-15_12-14-54/frequentWords.bin"

-- | 抽出データの基本ディレクトリ（extract-exeと同じ）
extractedDataBaseDir :: FilePath
extractedDataBaseDir = "extractedData"

-- | 抽出データのサブディレクトリプレフィックス（extract-exeと同じ）
extractedDataPrefix :: String
extractedDataPrefix = "pairs_"

-- | 使用方法メッセージ
usageMsg :: String
usageMsg = "Usage: eval-extract-exe <sessionId|directory> [modelPath] [frequentWordsPath]\n" ++
           "  sessionId: Session ID from extract-exe (e.g., D9T6000_dne_2025-12-21_12-00-00)\n" ++
           "             or full directory path (e.g., extractedData/pairs_D9T6000_dne_...)\n" ++
           "  modelPath: Path to trained model (default: " ++ defaultModelPath ++ ")\n" ++
           "  frequentWordsPath: Path to frequentWords.bin (default: " ++ defaultFrequentWordsPath ++ ")"

-- | extractedData/ 配下の利用可能なディレクトリを一覧
listExtractedDirs :: IO [FilePath]
listExtractedDirs = do
  exists <- doesDirectoryExist extractedDataBaseDir
  if not exists
    then return []
    else do
      allDirs <- listDirectory extractedDataBaseDir
      let pairsDirs = filter (extractedDataPrefix `isPrefixOf`) allDirs
      return $ reverse $ sort pairsDirs  -- 新しい順にソート

-- | セッションIDまたはディレクトリパスを解決
resolveExtractedDir :: String -> IO FilePath
resolveExtractedDir input = do
  -- まず入力をそのままディレクトリとして確認
  existsAsIs <- doesDirectoryExist input
  if existsAsIs
    then return input
    else do
      -- extractedData/pairs_<input> として確認
      let withPrefix = extractedDataBaseDir </> (extractedDataPrefix ++ input)
      existsWithPrefix <- doesDirectoryExist withPrefix
      if existsWithPrefix
        then return withPrefix
        else do
          -- extractedData/<input> として確認
          let inBaseDir = extractedDataBaseDir </> input
          existsInBase <- doesDirectoryExist inBaseDir
          if existsInBase
            then return inBaseDir
            else error $ "Directory not found: " ++ input ++ "\n" ++
                         "  Tried: " ++ input ++ ", " ++ withPrefix ++ ", " ++ inBaseDir

main :: IO ()
main = do
  args <- getArgs
  
  -- コマンドライン引数の解析
  (extractedDir, modelPath, frequentWordsPath) <- case args of
        [] -> do
          -- 引数なしの場合、利用可能なディレクトリを表示
          availableDirs <- listExtractedDirs
          if null availableDirs
            then error $ "No extracted data found in " ++ extractedDataBaseDir ++ "/\n" ++ usageMsg
            else do
              putStrLn "Available extracted data directories:"
              mapM_ (\d -> putStrLn $ "  " ++ d) availableDirs
              putStrLn ""
              error usageMsg
        [sessionIdOrDir] -> do
          dir <- resolveExtractedDir sessionIdOrDir
          return (dir, defaultModelPath, defaultFrequentWordsPath)
        [sessionIdOrDir, model] -> do
          dir <- resolveExtractedDir sessionIdOrDir
          return (dir, model, defaultFrequentWordsPath)
        [sessionIdOrDir, model, freqWords] -> do
          dir <- resolveExtractedDir sessionIdOrDir
          return (dir, model, freqWords)
        _ -> error usageMsg
  
  putStrLn "=== Evaluate Extracted Data ==="
  putStrLn $ "Extracted data directory: " ++ extractedDir
  putStrLn $ "Model path: " ++ modelPath
  putStrLn $ "FrequentWords path: " ++ frequentWordsPath
  putStrLn ""
  
  -- ファイルの存在確認
  modelExists <- doesFileExist modelPath
  freqWordsExists <- doesFileExist frequentWordsPath
  
  if not modelExists
    then error $ "Model file not found: " ++ modelPath
    else if not freqWordsExists
    then error $ "FrequentWords file not found: " ++ frequentWordsPath
    else return ()
  
  -- モデルとfrequentWordsをロード
  putStrLn "Loading model and frequentWords..."
  (model, wordMap) <- loadModelAndFrequentWords modelPath frequentWordsPath defaultHyperParams
  putStrLn "Model loaded successfully."
  putStrLn ""
  
  -- 抽出データディレクトリから.binファイルを取得
  allFiles <- listDirectory extractedDir
  let binFiles = filter (\f -> takeExtension f == ".bin") allFiles
  
  putStrLn $ "Found " ++ show (length binFiles) ++ " .bin files"
  putStrLn ""
  
  -- 各ファイルからデータをロード
  allData <- fmap concat $ forM binFiles $ \binFile -> do
    let filepath = extractedDir </> binFile
    putStrLn $ "Loading: " ++ binFile
    extractedData <- loadExtractedData filepath
    putStrLn $ "  Loaded " ++ show (length extractedData) ++ " pairs"
    return extractedData
  
  putStrLn ""
  putStrLn $ "Total loaded pairs: " ++ show (length allData)
  
  -- DTTruleからRuleLabelへ変換（変換できないものはフィルタリング）
  let convertedData = mapMaybe convertToRuleLabel allData
  putStrLn $ "Converted pairs (with valid RuleLabel): " ++ show (length convertedData)
  putStrLn ""
  
  if null convertedData
    then putStrLn "No valid data to evaluate."
    else do
      -- JudgmentをToken列に変換
      let delimiterToken = Unused
          testData = map (\(judgment, ruleLabel) -> 
                            (splitJudgment judgment wordMap delimiterToken, ruleLabel)
                         ) convertedData
      
      -- 評価を実行
      putStrLn "Running evaluation..."
      let device = dev defaultHyperParams
          biDirectional = bi_directional defaultHyperParams
      
      evalResult <- evaluateModel device model testData biDirectional
      
      -- 結果を表示
      putStrLn ""
      putStrLn "============================================"
      putStrLn "=== EVALUATION RESULTS ==="
      putStrLn "============================================"
      putStrLn ""
      
      -- 分類レポートを表示
      T.putStr $ TL.toStrict $ erClassificationReport evalResult
      
      putStrLn ""
      putStrLn $ "Total samples: " ++ show (length testData)
      putStrLn $ "Correct predictions: " ++ show (length $ filter id $ erCorrectFlags evalResult)
      putStrLn $ "Accuracy: " ++ show (erAccuracy evalResult)
      
      -- 結果をファイルに保存（extractedData/と同じ構造）
      now <- getCurrentTime
      let timestamp = formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" now
          -- extractedDirからセッション情報を取得（pairs_を除去）
          sessionInfo = let base = takeBaseName extractedDir
                        in if extractedDataPrefix `isPrefixOf` base
                           then drop (length extractedDataPrefix) base
                           else base
          outputDir = "evaluationResults" </> ("eval_" ++ sessionInfo ++ "_" ++ timestamp)
      
      createDirectoryIfMissing True outputDir
      
      let allLabels = enumFrom minBound :: [BR.RuleLabel]
      saveEvaluationReport outputDir evalResult allLabels
      
      putStrLn ""
      putStrLn $ "Results saved to: " ++ outputDir

