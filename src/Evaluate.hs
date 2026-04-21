{-# LANGUAGE RecordWildCards #-}

module Evaluate
  ( -- * 評価関数
    evaluateModel
  , evaluateWithPaths
  , EvaluationResult(..)
    -- * モデル・データのロード
  , loadModelAndFrequentWords
    -- * 予測関数
  , predictOnTestData
    -- * レポート出力
  , saveEvaluationReport
  ) where

import Control.Monad (forM)
import qualified Data.ByteString as B
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as E
import Data.Store (decode)

import qualified DTS.Prover.Wani.BackwardRules as BR
import Torch.Tensor (Tensor, asValue, asTensor, toDevice)
import Torch.Device (Device(..), DeviceType(..))
import Torch.Functional (Dim(..), argmax, KeepDim(..))
import Torch.NN (sample)
import Torch.Serialize (loadParams)

import ML.Exp.Chart (drawConfusionMatrix)
import ML.Exp.Classification (showClassificationReport)

import Forward (HypParams(..), Params, forward)
import SplitJudgment (Token(..), DelimiterToken(..), buildWordMap, WordMap)

-- | 評価結果を格納するデータ型
data EvaluationResult = EvaluationResult
  { erPredictions      :: [(BR.RuleLabel, BR.RuleLabel)]  -- ^ (予測, 正解) のペア
  , erCorrectFlags     :: [Bool]                          -- ^ 各予測が正しかったかどうか
  , erAccuracy         :: Double                          -- ^ 精度
  , erClassificationReport :: TL.Text                     -- ^ 分類レポート
  } deriving (Show)

-- | モデルとfrequentWordsをロードする
--
-- 引数:
-- * modelPath - モデルファイルのパス
-- * frequentWordsPath - frequentWordsファイルのパス
-- * hyperParams - モデルのハイパーパラメータ
--
-- 戻り値:
-- * (ロードされたモデル, WordMap)
loadModelAndFrequentWords :: FilePath -> FilePath -> HypParams -> IO (Params, WordMap)
loadModelAndFrequentWords modelPath frequentWordsPath hyperParams = do
  -- 空のモデルを初期化
  emptyModel <- sample hyperParams
  -- パラメータをロード
  model <- loadParams emptyModel modelPath
  
  -- frequentWordsをロード
  frequentWordsEither <- decode <$> B.readFile frequentWordsPath
  frequentWords <- case frequentWordsEither of
    Left err -> error $ "Failed to decode frequentWords: " ++ show err
    Right ws -> return ws
  
  -- WordMapを構築
  let wordMap = buildWordMap frequentWords
  
  return (model, wordMap)

-- | テストデータに対して予測を行う
--
-- 引数:
-- * device - 使用するデバイス
-- * model - 学習済みモデル
-- * testData - テストデータ (トークン列, 正解ラベル) のリスト
-- * biDirectional - 双方向LSTMかどうか
--
-- 戻り値:
-- * [(予測ラベル, 正解ラベル, 正解フラグ)]
predictOnTestData :: Device -> Params -> [([Token], BR.RuleLabel)] -> Bool 
                  -> IO [(BR.RuleLabel, BR.RuleLabel, Bool)]
predictOnTestData device model testData biDirectional = do
  forM testData $ \(tokens, groundTruthLabel) -> do
    let output' = forward device model tokens biDirectional
        groundTruthIndex = toDevice device (asTensor [(fromEnum groundTruthLabel) :: Int])
        predictedClassIndex = argmax (Dim 1) KeepDim output'
        isCorrect = groundTruthIndex == predictedClassIndex
        predictedLabel = toEnum (asValue predictedClassIndex :: Int) :: BR.RuleLabel
    return (predictedLabel, groundTruthLabel, isCorrect)

-- | モデルを評価する
--
-- 引数:
-- * device - 使用するデバイス
-- * model - 学習済みモデル
-- * testData - テストデータ
-- * biDirectional - 双方向LSTMかどうか
--
-- 戻り値:
-- * EvaluationResult
evaluateModel :: Device -> Params -> [([Token], BR.RuleLabel)] -> Bool -> IO EvaluationResult
evaluateModel device model testData biDirectional = do
  -- 予測を実行
  results <- predictOnTestData device model testData biDirectional
  
  let predictions = map (\(pred, gt, _) -> (pred, gt)) results
      correctFlags = map (\(_, _, correct) -> correct) results
      numCorrect = length $ filter id correctFlags
      accuracy = fromIntegral numCorrect / fromIntegral (length correctFlags)
      allLabels = enumFrom minBound :: [BR.RuleLabel]
      classReport = TL.fromStrict $ showClassificationReport (length allLabels) predictions
  
  return EvaluationResult
    { erPredictions = predictions
    , erCorrectFlags = correctFlags
    , erAccuracy = accuracy
    , erClassificationReport = classReport
    }

-- | ファイルパスを指定してモデルを評価する（便利関数）
--
-- 引数:
-- * modelPath - モデルファイルのパス
-- * frequentWordsPath - frequentWordsファイルのパス
-- * hyperParams - ハイパーパラメータ
-- * testData - テストデータ
-- * biDirectional - 双方向LSTMかどうか
--
-- 戻り値:
-- * EvaluationResult
evaluateWithPaths :: FilePath -> FilePath -> HypParams -> [([Token], BR.RuleLabel)] -> Bool 
                  -> IO EvaluationResult
evaluateWithPaths modelPath frequentWordsPath hyperParams testData biDirectional = do
  let device = dev hyperParams
  (model, _wordMap) <- loadModelAndFrequentWords modelPath frequentWordsPath hyperParams
  evaluateModel device model testData biDirectional

-- | 評価結果をファイルに保存する
--
-- 引数:
-- * outputDir - 出力ディレクトリ
-- * result - 評価結果
-- * allLabels - 全ラベルのリスト（混同行列用）
saveEvaluationReport :: FilePath -> EvaluationResult -> [BR.RuleLabel] -> IO ()
saveEvaluationReport outputDir result allLabels = do
  let classificationReportFile = outputDir ++ "/classification-report.txt"
      confusionMatrixFile = outputDir ++ "/confusion-matrix.png"
  
  -- 分類レポートを保存
  B.writeFile classificationReportFile (E.encodeUtf8 $ TL.toStrict $ erClassificationReport result)
  putStrLn $ "Classification report saved to: " ++ classificationReportFile
  
  -- 混同行列を描画・保存
  drawConfusionMatrix confusionMatrixFile (length allLabels) (erPredictions result)
  putStrLn $ "Confusion matrix saved to: " ++ confusionMatrixFile
  
  -- 精度を表示
  putStrLn $ "Accuracy: " ++ show (erAccuracy result)

