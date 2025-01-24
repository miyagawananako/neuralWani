{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

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
import           System.Environment (getArgs)
import System.Mem (performGC)
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
import SplitJudgment (Token(..), loadActionsFromBinary, getConstantSymbolsFromJudgment, getFrequentConstantSymbols, splitJudgment, DelimiterToken(..))

proofSearchResultFilePath :: FilePath
proofSearchResultFilePath = "data/proofSearchResult"

allLabels :: [QT.DTTrule]
allLabels = [minBound..]

allTokens :: [Token]
allTokens = [minBound..]

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
forward :: Device -> Params -> ([Token], QT.DTTrule) -> Bool -> IO Tensor
forward device Params{..} dataset bi_directional = do
  let inputIndices = map (\w -> fromEnum w :: Int) $ fst dataset
      idxs = toDevice device $ asTensor (inputIndices :: [Int])
      input = embedding' (transpose2D $ toDependent w_emb) idxs
      dropout_prob = Nothing
      (_, (h, _)) = lstmLayers lstm_params dropout_prob h0c0 $ input
  lastOutput <- extractLastOutput h bi_directional
  let output = linearLayer mlp_params lastOutput
      output' = logSoftmax (Dim 1) output
  pure output'

extractLastOutput :: Tensor -> Bool -> IO Tensor
extractLastOutput tensor bi_directional = do
  let shapeInput = shape tensor
  case bi_directional of 
    True -> do
      let lastOutput1 = sliceDim 0 (shapeInput !! 0 - 2) (shapeInput !! 0) 1 tensor  -- [2, hidden_size]
      return $ reshape [1, 2 * (shapeInput !! (length shapeInput - 1))] lastOutput1  -- [1, 2 * hidden_size]
    False -> do
      case shapeInput of
        [1, _] -> return tensor
        [_, _] -> return $ sliceDim 0 (shapeInput !! 0 - 1) (shapeInput !! 0) 1 tensor
        _      -> error $ "Unexpected shape: " ++ show shapeInput

countRule :: [QT.DTTrule] -> [(QT.DTTrule, Int)]
countRule rules = List.sortOn (Down . snd) $ Map.toList ruleFreqMap
  where
    ruleFreqMap :: Map.Map QT.DTTrule Int
    ruleFreqMap = foldr (\word acc -> Map.insertWith (+) word 1 acc) Map.empty rules

splitByLabel :: [([Token], QT.DTTrule)] -> IO [(QT.DTTrule, [([Token], QT.DTTrule)])]
splitByLabel dataset = do
  flip fix (0 :: Int, dataset, []) $ \loop (i, remainingData, splittedData) -> do
    if remainingData == [] then return splittedData
    else do
      let (tokens', rule) = head remainingData
          data' = (tokens', rule)
          splittedData' = Map.toList $ Map.insertWith (++) rule [data'] (Map.fromList splittedData)
      loop (i + 1, tail remainingData, splittedData')

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
  args <- getArgs
  let bi = read (args !! 0) :: Bool
      emb = read (args !! 1) :: Int
      h = read (args !! 2) :: Int
      l = read (args !! 3) :: Int
      bias = read (args !! 4) :: Bool
      lr = read (args !! 5) :: Float
      steps = read (args !! 6) :: Int
      iter = read (args !! 7) :: Int
      delimiterToken = read (args !! 8) :: DelimiterToken
  waniTestDataset <- loadActionsFromBinary proofSearchResultFilePath

  jsemFiles <- listDirectory "data/JSeM/"
  jsemDatasets <- mapM (\file -> loadActionsFromBinary ("data/JSeM/" </> file)) jsemFiles

  let dataset = waniTestDataset ++ concat jsemDatasets
      wordList = concatMap (\(judgment, _) -> getConstantSymbolsFromJudgment judgment) dataset
      frequentWords = getFrequentConstantSymbols wordList
      constructorData = map (\(judgment, _) -> splitJudgment judgment frequentWords delimiterToken) dataset
      ruleList = map (\(_, rule) -> rule) dataset

  let countedRules = countRule ruleList
  print $ "countedRules " ++ show countedRules
  splitedData <- splitByLabel (zip constructorData ruleList)
  crossValidationData <- splitDataForCrossValidation splitedData 450 iter

  currentTime <- getZonedTime
  let timeString = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d_%H-%M-%S" (zonedTimeToLocalTime currentTime)
      device = Device CUDA 0
      biDirectional = bi
      embDim = emb
      numOfLayers = l
      hiddenSize = h
      hasBias = bias
      vocabSize = length allTokens
      projSize = Nothing
      numOfRules = length allLabels
      hyperParams = HypParams device biDirectional embDim hasBias projSize vocabSize numOfLayers hiddenSize numOfRules
      learningRate = toDevice device (asTensor (lr :: Float))
      numberOfSteps = steps
  print $ "hyperParams " ++ show hyperParams
  print $ "learningRate " ++ show learningRate
  print $ "numberOfSteps " ++ show numberOfSteps
  print $ "iter " ++ show iter
  print $ "delimiterToken " ++ show delimiterToken
  initModel <- sample hyperParams
  let optimizer = mkAdam 0 0.9 0.999 (flattenParameters initModel)
  ((trainedModel), lossesPair) <- mapAccumM [1..iter] (initModel) $ \epoc (model) -> do
    let validData = crossValidationData !! (epoc - 1)
        trainData = concat $ take (epoc - 1) crossValidationData ++ drop epoc crossValidationData
    shuffledTrainData <- shuffleM trainData
    ((trainedModel', _, _), trainLossList) <- mapAccumM shuffledTrainData (model, 0, 0) $ \dat (mdl, currentSumLoss, i) -> do
      performGC
      output' <- forward device mdl dat biDirectional
      performGC
      let groundTruthIndex = toDevice device (asTensor [(fromEnum $ snd dat) :: Int])
          loss = nllLoss' groundTruthIndex output'
          lossValue = (asValue loss) :: Float
          sumLoss = currentSumLoss + loss
      if (i + 1) `mod` numberOfSteps == 0
        then do
          u <- update mdl optimizer sumLoss learningRate
          performGC
          let (newModel, _) = u
          print $ "epoch " ++ show epoc ++ " i " ++ show i ++ " trainingLoss " ++ show (asValue (sumLoss / fromIntegral numberOfSteps) :: Float)
          return ((newModel, 0, i + 1), lossValue)
        else do
          return ((mdl, sumLoss, i + 1), lossValue)
    pairs <- forM validData $ \dataPoint -> do
      validOutput' <- forward device trainedModel' dataPoint biDirectional
      performGC
      let groundTruthIndex' = toDevice device (asTensor [(fromEnum $ snd dataPoint) :: Int])
          loss' = nllLoss' groundTruthIndex' validOutput'
          validLossValue = (asValue loss') :: Float
          predictedClassIndex' = argmax (Dim 1) KeepDim validOutput'
          label = toEnum (asValue predictedClassIndex' :: Int) :: QT.DTTrule
      return (validLossValue, label)
    let (validLosses, predictedLabel) = unzip pairs
        validLoss = sum validLosses / fromIntegral (length validLosses)
        classificationReport = showClassificationReport (length allLabels) (zip predictedLabel (snd $ unzip $ validData))
        confusionMatrixFileName = "trained_data/confusion-matrix" ++ timeString ++ "_epoc" ++ show epoc ++ ".png"
        classificationReportFileName = "trained_data/classification-report" ++ timeString ++ "_epoc" ++ show epoc ++ ".txt"
    B.writeFile classificationReportFileName (E.encodeUtf8 classificationReport)
    drawConfusionMatrix confusionMatrixFileName (length allLabels) (zip predictedLabel (snd $ unzip $ validData))
    let avgTrainLoss = sum trainLossList / fromIntegral (length trainLossList)
    print $ "epoch " ++ show epoc ++ " avgTrainLoss " ++ show avgTrainLoss ++ " avgValidLoss " ++ show validLoss
    print "----------------"

    return (trainedModel', (avgTrainLoss, validLoss))

  let modelFileName = "trained_data/seq-class" ++ timeString ++ ".model"
      graphFileName = "trained_data/graph-seq-class" ++ timeString ++ ".png"
      learningCurveTitle = "type: " ++ show delimiterToken ++ " s: " ++ show numberOfSteps ++ " lr: " ++ show (asValue learningRate :: Float) ++  " i: " ++ show embDim ++ " h: " ++ show hiddenSize ++ " layer: " ++ show numOfLayers
      (losses, validLosses) = unzip lossesPair
  saveParams trainedModel modelFileName
  drawLearningCurve graphFileName learningCurveTitle [("training", reverse losses), ("validation", reverse validLosses)]