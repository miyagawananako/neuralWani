{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

import GHC.Generics                   --base
import Control.Monad (forM)
import Data.Function(fix)
import System.Random.Shuffle (shuffleM)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Data.Time.LocalTime
import qualified Data.Time as Time
import qualified Data.ByteString as B --bytestring
import qualified Data.Text.Encoding as E
import Data.Ord (Down(..))
import qualified Data.Map.Strict as Map
import qualified Data.List as List
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
    let d = if bi_directional then 2 else 1
    randomTensor1 <- randnIO' dev [d * num_layers, hidden_size]
    randomTensor2 <- randnIO' dev [d * num_layers, hidden_size]
    Params
      <$> sample (LstmHypParams dev bi_directional emb_dim hidden_size num_layers has_bias proj_size)
      <*> (makeIndependent =<< randnIO' dev [emb_dim, vocab_size])
      <*> sample (LinearHypParams dev has_bias (d * hidden_size) num_rules)
      <*> pure (0.01 * randomTensor1, 0.01 * randomTensor2)

-- | LSTMモデルの順伝播
-- | (loss, 予測クラスのインデックス)を返す
forward :: Device -> Params -> ([Token], QT.DTTrule) -> IO (Tensor, Tensor)
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
  pure (loss, predictedClassIndex)

extractLastOutput :: Tensor -> IO Tensor
extractLastOutput tensor = do
  let shapeInput = shape tensor
  case shapeInput of
    [1, n] -> return $ reshape [1, n] tensor
    [_, n] -> return $ reshape [1, n] $ sliceDim 0 (length shapeInput - 1) (length shapeInput) 1 tensor
    _      -> error $ "Unexpected shape: " ++ show shapeInput

countRule :: [QT.DTTrule] -> [(QT.DTTrule, Int)]
countRule rules = List.sortOn (Down . snd) $ Map.toList ruleFreqMap
  where
    ruleFreqMap :: Map.Map QT.DTTrule Int
    ruleFreqMap = foldr (\word acc -> Map.insertWith (+) word 1 acc) Map.empty rules

splitByLabel :: [([Token], QT.DTTrule)] -> IO [(QT.DTTrule, [([Token], QT.DTTrule)])]
splitByLabel dataset = do
  flip fix (0 :: Int, dataset, []) $ \loop (i, datalist, splittedData) -> do
    if datalist == [] then return splittedData
    else do
      let (tokens', rule) = head datalist
          data' = (tokens', rule)
          rest = tail datalist
          splittedData' = Map.toList $ Map.insertWith (++) rule [data'] (Map.fromList splittedData)
      loop (i + 1, rest, splittedData')

partition :: Int -> [a] -> [[a]]
partition n xs = go n xs
  where
    len = length xs
    baseSize = len `div` n
    remainder = len `mod` n

    -- Helper function to calculate size for current partition
    getSizeForPartition i
      | i <= remainder = baseSize + 1
      | otherwise      = baseSize

    -- Main recursive function
    go :: Int -> [a] -> [[a]]
    go parts lst
      | parts <= 0 = []
      | null lst   = []
      | parts == 1 = [lst]
      | otherwise  = currentPart : go (parts - 1) rest
      where
        currentSize = getSizeForPartition parts
        (currentPart, rest) = splitAt currentSize lst

splitDataForCrossValidation :: [(QT.DTTrule, [([Token], QT.DTTrule)])] -> Int -> Int -> IO [[([Token], QT.DTTrule)]]
splitDataForCrossValidation splittedDataByLabel threshold n = do
  let initialSplittedDataList = replicate n []
  flip fix (0 :: Int, splittedDataByLabel, initialSplittedDataList) $ \loop (i, datalist, splittedDataList) -> do
    if datalist == [] then return splittedDataList
    else do
      let (_, datas) = head datalist
          rest = tail datalist
      shuffledData <- shuffleM datas
      let takeThreshold = take threshold shuffledData
          splittedData = partition n takeThreshold
          newSplittedDataList = zipWith (++) splittedDataList splittedData
      loop (i + 1, rest, newSplittedDataList)

main :: IO()
main = do
  waniTestDataset <- loadActionsFromBinary proofSearchResultFilePath

  jsemFiles <- listDirectory "data/JSeM/"
  jsemDatasets <- mapM (\file -> loadActionsFromBinary ("data/JSeM/" </> file)) jsemFiles

  let dataset = waniTestDataset ++ concat jsemDatasets
      wordList = concatMap (\(judgment, _) -> getWordsFromJudgment judgment) dataset
      frequentWords = getFrequentWords wordList
      isParen = False
      isSep = False
      constructorData = map (\(judgment, _) -> splitJudgment judgment frequentWords isParen isSep) dataset
      ruleList = map (\(_, rule) -> rule) dataset

  let countedRules = countRule ruleList
  print $ "countedRules " ++ show countedRules
  splitedData <- splitByLabel (zip constructorData ruleList)
  let iter = 5 :: Int
  crossValidationData <- splitDataForCrossValidation splitedData 450 iter

  currentTime <- getZonedTime
  let timeString = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d_%H-%M-%S" (zonedTimeToLocalTime currentTime)
      device = Device CPU 0
      biDirectional = True
      embDim = 256
      numOfLayers = 2
      hiddenSize = 128
      hasBias = False
      vocabSize = length tokens
      projSize = Nothing
      numOfRules = length labels
      hyperParams = HypParams device biDirectional embDim hasBias projSize vocabSize numOfLayers hiddenSize numOfRules
      learningRate = 5e-4 :: Tensor
      numberOfSteps = 32
  initModel <- sample hyperParams
  let optimizer = mkAdam 0 0.9 0.999 (flattenParameters initModel)
  ((trainedModel), lossesPair) <- mapAccumM [1..iter] (initModel) $ \epoc (model) -> do
    let validData = crossValidationData !! (epoc - 1)
        trainData = concat $ take (epoc - 1) crossValidationData ++ drop epoc crossValidationData
    shuffledTrainData <- shuffleM trainData
    flip fix (0 :: Int, model, shuffledTrainData, 0, [], 0 :: Tensor) $ \loop (i, mdl, data_list, sumLossValue, validLossList, currentSumLoss) -> do
      if length data_list > 0 then do
        let (oneData, restDataList) = splitAt 1 data_list
        (loss, _) <- forward device mdl (head oneData)
        let lossValue = (asValue loss) :: Float
            sumLoss = currentSumLoss + loss
        if (i + 1) `mod` numberOfSteps == 0 then do
          u <- update mdl optimizer sumLoss learningRate
          let (newModel, _) = u
          pairs <- forM validData $ \dataPoint -> do
            (loss', predictedClassIndex) <- forward device mdl dataPoint
            let validLossValue = (asValue loss') :: Float
                label = toEnum (asValue predictedClassIndex :: Int) :: QT.DTTrule
            return (validLossValue, label)
          let (validLosses, predictedLabel) = unzip pairs
              validLoss = sum validLosses / fromIntegral (length validLosses)
              classificationReport = showClassificationReport (length labels) (zip predictedLabel (snd $ unzip $ validData))
          let confusionMatrixFileName = "trained_data/confusion-matrix" ++ timeString ++ "_epoc" ++ show epoc ++ ".png"
              classificationReportFileName = "trained_data/classification-report" ++ timeString ++ "_epoc" ++ show epoc ++ ".txt"
          B.writeFile classificationReportFileName (E.encodeUtf8 classificationReport)
          drawConfusionMatrix confusionMatrixFileName (length labels) (zip predictedLabel (snd $ unzip $ validData))
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

  let modelFileName = "trained_data/seq-class" ++ timeString ++ ".model"
      graphFileName = "trained_data/graph-seq-class" ++ timeString ++ ".png"
      splitType = if isParen then "()" else if isSep then "SEP" else "EO~"
      learningCurveTitle = "type: " ++ show splitType ++ " s: " ++ show numberOfSteps ++ " lr: " ++ show (asValue learningRate :: Float) ++  " i: " ++ show embDim ++ " h: " ++ show hiddenSize ++ " layer: " ++ show numOfLayers
      (losses, validLosses) = unzip lossesPair
  saveParams trainedModel modelFileName
  drawLearningCurve graphFileName learningCurveTitle [("training", reverse losses), ("validation", reverse validLosses)]