{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

import GHC.Generics                   --base
import qualified DTS.QueryTypes as QT
import Control.Monad (forM)
import Data.Function(fix)
import System.Random.Shuffle (shuffleM)
--hasktorch
import Torch.Tensor       (Tensor(..),asValue,reshape, shape, asTensor, sliceDim)
import Torch.Device       (Device(..),DeviceType(..))
import Torch.Functional   (Dim(..),cat, softmax, matmul,nllLoss',argmax,KeepDim(..), transpose2D, binaryCrossEntropyLoss', stack, embedding', logSoftmax)
import Torch.NN           (Parameter,Parameterized,Randomizable,sample)
import Torch.Autograd     (IndependentTensor(..),makeIndependent)
import Torch.Optim        (GD(..))
import Torch.Train        (update,showLoss,saveParams,loadParams)
import Torch.Control      (mapAccumM)
import Torch.Tensor.TensorFactories (randnIO')
import Torch.Layer.Linear (LinearHypParams(..),LinearParams,linearLayer)
import Torch.Layer.LSTM   (LstmHypParams(..),LstmParams,lstmLayers)
import ML.Util.Dict    (sortWords,oneHotFactory) --nlp-tools
import ML.Exp.Chart   (drawLearningCurve) --nlp-tools
import ML.Exp.Classification (showClassificationReport) --nlp-tools
import SplitJudgment (Token(..), loadActionsFromBinary, getWordsFromJudgment, getFrequentWords, splitJudgment)

saveFilePath :: FilePath
saveFilePath = "data/proofSearchResult"

-- TODO: minBoundを使えるようにlightblueをupdateする
labels :: [QT.DTTrule]
labels = [minBound..]
-- labels = [QT.Var, QT.Con]

tokens :: [Token]
tokens = [minBound..]
-- tokens = [Word1,COMMA,Type',Word2]

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

forward :: Params -> ([Token], QT.DTTrule) -> IO (Tensor, (Tensor, Tensor))
forward model dataset = do
  let inputIndices = map (\w -> fromEnum w :: Int) $ fst dataset
      idxs = asTensor (inputIndices :: [Int])
      input = embedding' (transpose2D $ toDependent (w_emb model)) idxs
      lstm = lstmLayers (lstmParams model)
      dropout_prob = Nothing
      (lstmOutput, newState) = lstm dropout_prob (h0c0 model) $ input
  pure (lstmOutput, newState)

predict :: Params -> ([Token], QT.DTTrule) -> (QT.DTTrule -> [Float]) -> IO (Tensor, Bool, (Tensor, Tensor))
predict model dataset oneHotLabels = do
  let groundTruth = asTensor (oneHotLabels $ snd dataset)
  let groundTruth' = argmax (Dim 0) KeepDim groundTruth
  (lstmOutput, newState) <- forward model dataset
  let mlp = linearLayer (mlpParams model)
  let output = mlp $ lstmOutput
  let shapeOutput = shape output
  let y' = case shapeOutput of
        [1, n] -> softmax (Dim 0) (reshape [n] output)
        [_, n] -> softmax (Dim 0) (reshape [n] $ sliceDim 0 (length shapeOutput - 1) (length shapeOutput) 1 output)
        _      -> error $ "Unexpected shape: " ++ show shapeOutput
  let output' = case shapeOutput of
        [1, n] -> logSoftmax (Dim 1) (reshape [1, n] output)
        [_, n] -> logSoftmax (Dim 1) (reshape [1, n] $ sliceDim 0 (length shapeOutput - 1) (length shapeOutput) 1 output)
        _      -> error $ "Unexpected shape: " ++ show shapeOutput
  let loss = nllLoss' groundTruth' output'
  let classLabels = argmax (Dim 0) KeepDim y'
  let isCorrect = groundTruth' == classLabels
  pure (loss, isCorrect, newState)

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

  let iter = 1 :: Int
      device = Device CPU 0
      biDirectional = False
      input_size = 2
      numOfLayers = 1
      hiddenSize = 7
      has_bias = False
      vocabSize = length tokens -- TODO: あっているのか確認する
      proj_size = Nothing
      (oneHotLabels, numOfRules) = oneHotFactory labels
      hyperParams = HypParams device biDirectional input_size has_bias proj_size vocabSize numOfLayers hiddenSize numOfRules
      learningRate = 1e-5 :: Tensor
      graphFileName = "app/train/graph-seq-class.png"
      modelFileName = "app/train/seq-class.model"
  initModel <- sample hyperParams
  ((trainedModel), lossesPair) <- mapAccumM [1..iter] (initModel) $ \epoc (model) -> do
    flip fix (0, model, trainData, 0) $ \loop (i, mdl, data_list, sumLossValue) -> do
      if length data_list > 0 then do
        let (oneData, restDataList) = splitAt 1 data_list
        (loss, _, newState) <- predict mdl (head oneData) oneHotLabels
        let lossValue = (asValue loss) :: Float
        print $ "epoch " ++ show epoc  ++ " i " ++ show i ++ " loss " ++ show loss
        let model' = mdl { h0c0 = newState }
        u <- update model' GD loss learningRate
        let (newModel, _) = u
        loop (i + 1, newModel, restDataList, sumLossValue + lossValue)
      else do
        validLosses <- forM validData $ \dataPoint -> do
          (loss, _, _) <- predict model dataPoint oneHotLabels
          let lossValue = (asValue loss) :: Float
          return lossValue

        let avgTrainLoss = sumLossValue / fromIntegral (length trainData)
            avgValidLoss = sum validLosses / fromIntegral (length validLosses)

        return (mdl, (avgTrainLoss, avgValidLoss))

  let (losses, validLosses) = unzip lossesPair
  saveParams trainedModel modelFileName
  drawLearningCurve graphFileName "Learning Curve" [("training", reverse losses), ("validation", reverse validLosses)]

  isCorrects <- forM testData $ \dataPoint -> do
    (_, isCorrect, _) <- predict trainedModel dataPoint oneHotLabels
    return isCorrect

  print $ "isCorrects " ++ show isCorrects

  let accuracy = fromIntegral (length (filter id isCorrects)) / fromIntegral (length isCorrects)
  print $ "Accuracy: " ++ show accuracy