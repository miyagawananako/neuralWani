{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

import GHC.Generics                   --base
import Control.Monad (forM)
import Data.Function(fix)
import System.Random.Shuffle (shuffleM)
import qualified Data.Text as T       --text
import qualified Data.Text.IO as T    --text
import qualified Data.Serialize.Text as T --cereal-text
import qualified Data.List as L       --base
import qualified DTS.QueryTypes as QT
--hasktorch
import Torch.Tensor       (Tensor(..),asValue,reshape, shape, asTensor, sliceDim, toDevice)
import Torch.Device       (Device(..),DeviceType(..))
import Torch.Functional   (Dim(..),cat, softmax, matmul,nllLoss',argmax,KeepDim(..), transpose2D, binaryCrossEntropyLoss', stack, embedding', logSoftmax)
import Torch.NN           (Parameter,Parameterized,Randomizable,sample, flattenParameters)
import Torch.Autograd     (IndependentTensor(..),makeIndependent)
import Torch.Optim        (GD(..), Adam(..), mkAdam)
import Torch.Train        (update,showLoss,saveParams,loadParams)
import Torch.Control      (mapAccumM)
import Torch.Tensor.TensorFactories (randnIO', asTensor'')
import Torch.Layer.Linear (LinearHypParams(..),LinearParams,linearLayer)
import Torch.Layer.LSTM   (LstmHypParams(..),LstmParams,lstmLayers)
import ML.Util.Dict    (sortWords,oneHotFactory) --nlp-tools
import ML.Exp.Chart   (drawLearningCurve) --nlp-tools
import ML.Exp.Classification (showClassificationReport) --nlp-tools
import SplitJudgment (Token(..), loadActionsFromBinary, getWordsFromJudgment, getFrequentWords, splitJudgment)

saveFilePath :: FilePath
saveFilePath = "data/proofSearchResult"

labels :: [QT.DTTrule]
labels = [minBound..]

tokens :: [Token]
tokens = [minBound..]

-- 初期化のためのハイパーパラメータ
data HypParams = HypParams {
  dev :: Device,
  bi_directional :: Bool,
  input_size :: Int,
  has_bias :: Bool,
  proj_size :: Maybe Int,
  vocab_size :: Int,
  num_layers :: Int,
  hidden_size :: Int,
  num_rules :: Int
  } deriving (Eq, Show)

-- 学習されるパラメータmodelはこの型
data Params = Params {
  lstmParams :: LstmParams,
  w_emb :: Parameter,
  mlpParams :: LinearParams,
  h0c0 :: (Tensor, Tensor)
  } deriving (Show, Generic)

instance Parameterized Params

-- data Paramsをここでつくる
instance Randomizable HypParams Params where
  sample HypParams{..} = do
    randomTensor1 <- randnIO' dev [num_layers, hidden_size]
    randomTensor2 <- randnIO' dev [num_layers, hidden_size]
    Params
      <$> sample (LstmHypParams dev bi_directional input_size hidden_size num_layers has_bias proj_size)
      <*> (makeIndependent =<< randnIO' dev [input_size, vocab_size])
      <*> sample (LinearHypParams dev has_bias hidden_size num_rules)
      <*> pure (0.01 * randomTensor1, 0.01 * randomTensor2)

forward :: Device -> Params -> ([Token], QT.DTTrule) -> IO (Tensor, (Tensor, Tensor))
forward device model dataset = do
  let inputIndices = map (\w -> fromEnum w :: Int) $ fst dataset
      idxs = asTensor (inputIndices :: [Int])
      toDeviceIdxs = toDevice device idxs
      input = embedding' (transpose2D $ toDependent (w_emb model)) toDeviceIdxs
      dropout_prob = Nothing
      (lstmOutput, newState) = lstmLayers (lstmParams model) dropout_prob (h0c0 model) $ input
  pure (lstmOutput, newState)

predict :: Device -> Params -> ([Token], QT.DTTrule) -> (QT.DTTrule -> [Float]) -> IO (Tensor, Bool, Tensor, (Tensor, Tensor))
predict device model dataset oneHotLabels = do
  let groundTruthOneHot = asTensor'' device (tail $ oneHotLabels $ snd dataset)
      groundTruthIndex = argmax (Dim 0) KeepDim groundTruthOneHot
  (lstmOutput, newState) <- forward device model dataset
  let output = linearLayer (mlpParams model) $ lstmOutput
  reshapedOutput <- reshapeTensor output
  let output' = logSoftmax (Dim 1) reshapedOutput
      loss = nllLoss' groundTruthIndex output'
      predictedClassIndex = argmax (Dim 1) KeepDim reshapedOutput
      isCorrect = groundTruthIndex == predictedClassIndex
  pure (loss, isCorrect, predictedClassIndex, newState)

reshapeTensor :: Tensor -> IO Tensor
reshapeTensor tensor = do
  let shapeInput = shape tensor
  case shapeInput of
    [1, n] -> return $ reshape [1, n] tensor
    [_, n] -> return $ reshape [1, n] $ sliceDim 0 (length shapeInput - 1) (length shapeInput) 1 tensor
    _      -> error $ "Unexpected shape: " ++ show shapeInput

main :: IO()
main = do
  dataset <- loadActionsFromBinary saveFilePath

  let wordList = concatMap (\(judgment, _) -> getWordsFromJudgment judgment) dataset
  let frequentWords = getFrequentWords wordList

  let constructorData = map (\(judgment, _) -> splitJudgment judgment frequentWords) dataset

  let ruleList = map (\(_, rule) -> rule) dataset

  allData <- shuffleM $ zip constructorData ruleList
  let (trainData, restData) = splitAt (length allData * 7 `div` 10) allData
  let (validData, testData) = splitAt (length restData * 5 `div` 10) restData

  let iter = 10 :: Int
      device = Device CUDA 0
      biDirectional = False
      input_size = 128
      numOfLayers = 1
      hiddenSize = 128
      has_bias = False
      vocabSize = length tokens
      proj_size = Nothing
      (oneHotLabels, _) = oneHotFactory labels
      numOfRules = length labels
      hyperParams = HypParams device biDirectional input_size has_bias proj_size vocabSize numOfLayers hiddenSize numOfRules
      learningRate = 1e-3 :: Tensor
      batchSize = 10
      graphFileName = "app/train/graph-seq-class.png"
      modelFileName = "app/train/seq-class.model"
  initModel <- sample hyperParams
  let optimizer = mkAdam 0 0.9 0.999 (flattenParameters initModel)
  ((trainedModel), lossesPair) <- mapAccumM [1..iter] (initModel) $ \epoc (model) -> do
    shuffledTrainData <- shuffleM trainData
    flip fix (0 :: Int, model, shuffledTrainData, 0, 0 :: Tensor) $ \loop (i, mdl, data_list, sumLossValue, currentSumLoss) -> do
      if length data_list > 0 then do
        let (oneData, restDataList) = splitAt 1 data_list
        (loss, _, _, newState) <- predict device mdl (head oneData) oneHotLabels
        let lossValue = (asValue loss) :: Float
        let model' = mdl { h0c0 = newState }
            sumLoss = currentSumLoss + loss
        if (i + 1) `mod` batchSize == 0 then do
          u <- update model' optimizer sumLoss learningRate
          let (newModel, _) = u
          print $ "epoch " ++ show epoc ++ " i " ++ show i ++ " avgloss " ++ show (sumLoss / fromIntegral batchSize)
          loop (i + 1, newModel, restDataList, sumLossValue + lossValue, 0)
        else do
          loop (i + 1, model', restDataList, sumLossValue + lossValue, sumLoss)
      else do
        validLosses <- forM validData $ \dataPoint -> do
          (loss, _, _, _) <- predict device mdl dataPoint oneHotLabels
          let lossValue = (asValue loss) :: Float
          return lossValue

        let avgTrainLoss = sumLossValue / fromIntegral (length trainData)
            avgValidLoss = sum validLosses / fromIntegral (length validLosses)

        return (mdl, (avgTrainLoss, avgValidLoss))

  let (losses, validLosses) = unzip lossesPair
  saveParams trainedModel modelFileName
  drawLearningCurve graphFileName "Learning Curve" [("training", reverse losses), ("validation", reverse validLosses)]

  pairs <- forM testData $ \dataPoint -> do
    (_, isCorrect, predictedClassIndex, _) <- predict device trainedModel dataPoint oneHotLabels
    let label = toEnum (asValue predictedClassIndex :: Int) :: QT.DTTrule
    return (isCorrect, label)

  let (isCorrects, ans) = unzip pairs

  print $ zip (snd $ unzip $ testData) ans

  -- T.putStr $ showClassificationReport $ zip (snd $ unzip $ testData) ans

  print $ "isCorrects " ++ show isCorrects

  let accuracy = fromIntegral (length (filter id isCorrects)) / fromIntegral (length isCorrects)
  print $ "Accuracy: " ++ show accuracy