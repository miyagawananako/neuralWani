{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

import GHC.Generics                   --base
import Control.Monad (forM)
import Data.Function(fix)
import System.Random.Shuffle (shuffleM)
import qualified Data.Text.IO as T    --text
import Data.Time.LocalTime
import qualified Data.Time as Time
import qualified Data.ByteString as B --bytestring
import qualified Data.Text.Encoding as E
import qualified DTS.QueryTypes as QT
--hasktorch
import Torch.Tensor       (Tensor(..),asValue,reshape, shape, asTensor, sliceDim, toDevice)
import Torch.Device       (Device(..),DeviceType(..))
import Torch.Functional   (Dim(..),nllLoss',argmax,KeepDim(..), transpose2D, embedding', logSoftmax)
import Torch.NN           (Parameter,Parameterized,Randomizable,sample, flattenParameters)
import Torch.Autograd     (IndependentTensor(..),makeIndependent)
import Torch.Optim        (mkAdam)
import Torch.Train        (update,saveParams,loadParams)
import Torch.Control      (mapAccumM)
import Torch.Tensor.TensorFactories (randnIO')
import Torch.Layer.Linear (LinearHypParams(..),LinearParams,linearLayer)
import Torch.Layer.LSTM   (LstmHypParams(..),LstmParams,lstmLayers)
import ML.Exp.Chart   (drawLearningCurve, drawConfusionMatrix) --nlp-tools
import ML.Exp.Classification (showClassificationReport) --nlp-tools
import SplitJudgment (Token(..), loadActionsFromBinary, getWordsFromJudgment, getFrequentWords, splitJudgment)

proofSearchResultFilePath :: FilePath
proofSearchResultFilePath = "data/proofSearchResult"

labels :: [QT.DTTrule]
labels = [minBound..]

tokens :: [Token]
tokens = [minBound..]

-- 初期化のためのハイパーパラメータ
data HypParams = HypParams {
  dev :: Device,
  bi_directional :: Bool,
  emb_dim :: Int,
  has_bias :: Bool,
  proj_size :: Maybe Int,
  vocab_size :: Int,
  num_layers :: Int,
  hidden_size :: Int,
  num_rules :: Int
  } deriving (Eq, Show)

data Params = Params {
  lstm_params :: LstmParams,
  w_emb :: Parameter,
  mlp_params :: LinearParams,
  h0c0 :: (Tensor, Tensor)
  } deriving (Show, Generic)

instance Parameterized Params

-- Paramsを初期化する
instance Randomizable HypParams Params where
  sample HypParams{..} = do
    randomTensor1 <- randnIO' dev [num_layers, hidden_size]
    randomTensor2 <- randnIO' dev [num_layers, hidden_size]
    Params
      <$> sample (LstmHypParams dev bi_directional emb_dim hidden_size num_layers has_bias proj_size)
      <*> (makeIndependent =<< randnIO' dev [emb_dim, vocab_size])
      <*> sample (LinearHypParams dev has_bias hidden_size num_rules)
      <*> pure (0.01 * randomTensor1, 0.01 * randomTensor2)

-- | LSTMモデルの順伝播
-- | (loss, 予測クラスと正解クラスが一致しているかどうか, 予測クラスのインデックス)を返す
forward :: Device -> Params -> ([Token], QT.DTTrule) -> IO (Tensor, Bool, Tensor)
forward device model dataset = do
  let groundTruthIndex = toDevice device (asTensor [(fromEnum $ snd dataset) :: Int])
      inputIndices = map (\w -> fromEnum w :: Int) $ fst dataset
      idxs = asTensor (inputIndices :: [Int])
      toDeviceIdxs = toDevice device idxs
      input = embedding' (transpose2D $ toDependent (w_emb model)) toDeviceIdxs
      dropout_prob = Nothing
      (lstmOutput, _) = lstmLayers (lstm_params model) dropout_prob (h0c0 model) $ input
  let output = linearLayer (mlp_params model) $ lstmOutput
  lastOutput <- extractLastOutput output
  let output' = logSoftmax (Dim 1) lastOutput
      loss = nllLoss' groundTruthIndex output'
      predictedClassIndex = argmax (Dim 1) KeepDim lastOutput
      isCorrect = groundTruthIndex == predictedClassIndex
  pure (loss, isCorrect, predictedClassIndex)

extractLastOutput :: Tensor -> IO Tensor
extractLastOutput tensor = do
  let shapeInput = shape tensor
  case shapeInput of
    [1, n] -> return $ reshape [1, n] tensor
    [_, n] -> return $ reshape [1, n] $ sliceDim 0 (length shapeInput - 1) (length shapeInput) 1 tensor
    _      -> error $ "Unexpected shape: " ++ show shapeInput

main :: IO()
main = do
  waniTestDataset <- loadActionsFromBinary proofSearchResultFilePath
  typeCheckTreesDataset <- loadActionsFromBinary "data/typeCheckTrees"
  let dataset = waniTestDataset ++ typeCheckTreesDataset
      wordList = concatMap (\(judgment, _) -> getWordsFromJudgment judgment) dataset
      frequentWords = getFrequentWords wordList
      isParen = False
      isSep = False
      constructorData = map (\(judgment, _) -> splitJudgment judgment frequentWords isParen isSep) dataset
      ruleList = map (\(_, rule) -> rule) dataset

  allData <- shuffleM $ zip constructorData ruleList
  let (trainData, restData) = splitAt (length allData * 7 `div` 10) allData
      (validData, testData) = splitAt (length restData * 5 `div` 10) restData

  let iter = 10 :: Int
      device = Device CPU 0
      biDirectional = False
      embDim = 128
      numOfLayers = 1
      hiddenSize = 128
      hasBias = False
      vocabSize = length tokens
      projSize = Nothing
      numOfRules = length labels
      hyperParams = HypParams device biDirectional embDim hasBias projSize vocabSize numOfLayers hiddenSize numOfRules
      learningRate = 1e-3 :: Tensor
      numberOfSteps = 32
  initModel <- sample hyperParams
  let optimizer = mkAdam 0 0.9 0.999 (flattenParameters initModel)
  ((trainedModel), lossesPair) <- mapAccumM [1..iter] (initModel) $ \epoc (model) -> do
    shuffledTrainData <- shuffleM trainData
    flip fix (0 :: Int, model, shuffledTrainData, 0, [], 0 :: Tensor) $ \loop (i, mdl, data_list, sumLossValue, validLossList, currentSumLoss) -> do
      if length data_list > 0 then do
        let (oneData, restDataList) = splitAt 1 data_list
        (loss, _, _) <- forward device mdl (head oneData)
        let lossValue = (asValue loss) :: Float
            sumLoss = currentSumLoss + loss
        if (i + 1) `mod` numberOfSteps == 0 then do
          u <- update mdl optimizer sumLoss learningRate
          let (newModel, _) = u
          validLosses <- forM validData $ \dataPoint -> do
            (loss', _, _) <- forward device mdl dataPoint
            let validLossValue = (asValue loss') :: Float
            return validLossValue
          let validLoss = sum validLosses / fromIntegral (length validLosses)
          print $ "epoch " ++ show epoc ++ " i " ++ show i ++ " trainingLoss " ++ show (asValue (sumLoss / fromIntegral numberOfSteps) :: Float) ++ " validLoss " ++ show validLoss
          loop (i + 1, newModel, restDataList, sumLossValue + lossValue, validLossList ++ [validLoss], 0)
        else do
          loop (i + 1, mdl, restDataList, sumLossValue + lossValue, validLossList, sumLoss)
      else do
        let avgTrainLoss = sumLossValue / fromIntegral (length trainData)
            avgValidLoss = sum validLossList / fromIntegral (length validLossList)
        print $ "epoch " ++ show epoc ++ " avgTrainLoss " ++ show avgTrainLoss ++ " avgValidLoss " ++ show avgValidLoss
        print "----------------"

        return (mdl, (avgTrainLoss, avgValidLoss))

  currentTime <- getZonedTime
  let timeString = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d_%H-%M-%S" (zonedTimeToLocalTime currentTime)
      modelFileName = "trained_data/seq-class" ++ timeString ++ ".model"
      graphFileName = "trained_data/graph-seq-class" ++ timeString ++ ".png"
      confusionMatrixFileName = "trained_data/confusion-matrix" ++ timeString ++ ".png"
      classificationReportFileName = "trained_data/classification-report" ++ timeString ++ ".txt"
      splitType = if isParen then "()" else if isSep then "SEP" else "EO~"
      learningCurveTitle = "type: " ++ show splitType ++ " s: " ++ show numberOfSteps ++ " lr: " ++ show (asValue learningRate :: Float) ++  " i: " ++ show embDim ++ " h: " ++ show hiddenSize ++ " layer: " ++ show numOfLayers
      (losses, validLosses) = unzip lossesPair
  saveParams trainedModel modelFileName
  drawLearningCurve graphFileName learningCurveTitle [("training", reverse losses), ("validation", reverse validLosses)]

  pairs <- forM testData $ \dataPoint -> do
    (_, isCorrect, predictedClassIndex) <- forward device trainedModel dataPoint
    let label = toEnum (asValue predictedClassIndex :: Int) :: QT.DTTrule
    return (isCorrect, label)

  let (isCorrects, predictedLabel) = unzip pairs

  print $ zip predictedLabel (snd $ unzip $ testData)

  let classificationReport = showClassificationReport (length labels) (zip predictedLabel (snd $ unzip $ testData))
  T.putStr classificationReport

  B.writeFile classificationReportFileName (E.encodeUtf8 classificationReport)

  drawConfusionMatrix confusionMatrixFileName (length labels) (zip predictedLabel (snd $ unzip $ testData))

  print $ "isCorrects " ++ show isCorrects

  let accuracy = (fromIntegral (length (filter id isCorrects)) / fromIntegral (length isCorrects)) :: Double
  print $ "Accuracy: " ++ show accuracy