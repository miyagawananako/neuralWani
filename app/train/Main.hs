{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (forM, when)
import System.Random.Shuffle (shuffleM)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import Data.Time.LocalTime
import qualified Data.Time as Time
import qualified Data.ByteString as B
import Data.Store (encode)
import qualified Data.List.Split as List
import System.Environment (getArgs)
import System.Mem (performGC)
import System.Directory (createDirectoryIfMissing)
import qualified DTS.Prover.Wani.BackwardRules as BR
import Data.Maybe (mapMaybe)

import Torch.Tensor (Tensor(..), asValue, asTensor, toDevice)
import Torch.Device (Device(..), DeviceType(..))
import Torch.Functional (nllLoss')
import Torch.NN (sample, flattenParameters)
import Torch.Optim (mkAdam)
import Torch.Train (update, saveParams)
import Torch.Control (mapAccumM)

import ML.Exp.Chart (drawLearningCurve)

import SplitJudgment (Token(..), getConstantSymbolsFromJudgment, getFrequentConstantSymbols, splitJudgment, DelimiterToken(..), dttruleToRuleLabel, buildWordMap)
import Forward (HypParams(..), Params(..), forward)
import Evaluate (evaluateModel, saveEvaluationReport, EvaluationResult(..))
import Training.Shared (backwardRules, formationRules, countRule, splitByLabel, smoothData, parseDatasetType, loadDataset)

allLabels :: [BR.RuleLabel]
allLabels = [minBound..]

allTokens :: [Token]
allTokens = [minBound..]

trainModel :: Device -> HypParams -> [([Token], BR.RuleLabel)] -> [([Token], BR.RuleLabel)] -> Bool -> Int -> Int -> Tensor -> [TL.Text] -> IO (Params, [(Float, Float)], [TL.Text])
trainModel device hyperParams trainData validData biDirectional iter numberOfBatch learningRate frequentWords = do
  initModel <- sample hyperParams
  let optimizer = mkAdam 0 0.9 0.999 (flattenParameters initModel)

  (trainedModel, lossesPair) <- mapAccumM [1..iter] initModel $ \epoc model -> do
    shuffledTrainData <- shuffleM trainData
    let batchedTrainData = List.chunksOf numberOfBatch shuffledTrainData
        batchedTrainData' = if null batchedTrainData
                              then []
                              else if length (last batchedTrainData) < numberOfBatch
                                     then init batchedTrainData
                                     else batchedTrainData

    ((trainedModel', _), lossPair) <- mapAccumM batchedTrainData' (model, 0 :: Int) $ \dataList (mdl, i) -> do
      performGC
      (sumLoss', losses) <- mapAccumM dataList (0 :: Tensor) $ \dat accumulatedLoss -> do
        let output' = forward device mdl (fst dat) biDirectional
        performGC
        let groundTruthIndex = toDevice device (asTensor [fromEnum (snd dat) :: Int])
            loss = nllLoss' groundTruthIndex output'
            lossValue = asValue loss :: Float
            sumLoss = accumulatedLoss + loss
        pure (sumLoss, lossValue)

      (newModel, _) <- update mdl optimizer sumLoss' learningRate
      performGC

      validLosses <- forM validData $ \dataPoint -> do
        let validOutput' = forward device mdl (fst dataPoint) biDirectional
        performGC
        let groundTruthIndex' = toDevice device (asTensor [fromEnum (snd dataPoint) :: Int])
            validLossValue = asValue (nllLoss' groundTruthIndex' validOutput') :: Float
        pure validLossValue

      let validLoss = if null validLosses then 0 else sum validLosses / fromIntegral (length validLosses)
          trainLoss = if null losses then 0 else sum losses / fromIntegral (length losses)
      print $ "epoch " ++ show epoc ++ " i " ++ show i ++ " trainingLoss " ++ show trainLoss ++ " validLoss " ++ show validLoss
      pure ((newModel, i + 1), (trainLoss, validLoss))

    let (trainLoss', validLoss') = unzip lossPair
        avgTrainLoss = if null trainLoss' then 0 else sum trainLoss' / fromIntegral (length trainLoss')
        avgValidLoss = if null validLoss' then 0 else sum validLoss' / fromIntegral (length validLoss')
    print $ "epoch " ++ show epoc ++ " avgTrainLoss " ++ show avgTrainLoss ++ " avgValidLoss " ++ show avgValidLoss
    print "----------------"

    pure (trainedModel', (avgTrainLoss, avgValidLoss))

  pure (trainedModel, lossesPair, frequentWords)

main :: IO ()
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
      datasetTypeStr = args !! 9
      datasetType = parseDatasetType datasetTypeStr

  print $ "Loading dataset: " ++ show datasetType
  (originalDataset, usedBinFiles) <- loadDataset datasetType

  let isIncludeF = False
      isOnlyBackwardRules = True
      backwardDataset = if isOnlyBackwardRules
        then filter (\(_, rule) -> elem rule backwardRules) originalDataset
        else originalDataset
      filteredDataset = if isIncludeF
        then backwardDataset
        else filter (\(_, rule) -> rule `notElem` formationRules) backwardDataset
      dataset = mapMaybe (\(judgment, dttRule) ->
                  case dttruleToRuleLabel dttRule of
                    Just ruleLabel -> Just (judgment, ruleLabel)
                    Nothing -> Nothing) filteredDataset
      wordList = concatMap (getConstantSymbolsFromJudgment . fst) dataset
      frequentWords = getFrequentConstantSymbols wordList
      wordMap = buildWordMap frequentWords
      constructorData = map (\(judgment, _) -> splitJudgment judgment wordMap delimiterToken) dataset
      ruleList = map snd dataset

  let countedRules = countRule ruleList
  print $ "countedRules " ++ show countedRules

  splittedData <- splitByLabel (zip constructorData ruleList)
  (trainData, validData, testData) <- smoothData splittedData Nothing

  let countedTrainRules = countRule $ map snd trainData
  print $ "countedRules (training data) " ++ show countedTrainRules

  let device = Device CUDA 0
      biDirectional = bi
      embDim = emb
      numOfLayers = l
      hiddenSize = h
      hasBias = bias
      vocabSize = length allTokens
      projSize = Nothing
      numOfRules = length (enumFrom minBound :: [BR.RuleLabel])
      hyperParams = HypParams device biDirectional embDim hasBias projSize vocabSize numOfLayers hiddenSize numOfRules
      learningRate = toDevice device (asTensor (lr :: Float))
      numberOfBatch = steps

  print $ "hyperParams " ++ show hyperParams
  print $ "learningRate " ++ show learningRate
  print $ "numberOfBatch " ++ show numberOfBatch
  print $ "iter " ++ show iter
  print $ "delimiterToken " ++ show delimiterToken

  startTime <- Time.getCurrentTime
  print $ "Training started at: " ++ show startTime

  (trainedModel, lossesPair, frequentWords') <- trainModel device hyperParams trainData validData biDirectional iter numberOfBatch learningRate frequentWords

  endTime <- Time.getCurrentTime
  let trainingDuration = Time.diffUTCTime endTime startTime
  print $ "Training finished at: " ++ show endTime
  print $ "Total training time: " ++ show trainingDuration

  currentTime <- getZonedTime
  let timeString = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d_%H-%M-%S" (zonedTimeToLocalTime currentTime)
      baseFolderName = case (isIncludeF, isOnlyBackwardRules) of
                        (True, True)   -> "trainedDataBackward"
                        (True, False)  -> "trainedData"
                        (False, True)  -> "trainedDataBackwardWithoutF"
                        (False, False) -> "trainedDataWithoutF"
      newFolderPath = baseFolderName ++ "/type" ++ show delimiterToken ++ "_bi" ++ show biDirectional ++ "_s" ++ show numberOfBatch ++ "_lr" ++ show (asValue learningRate :: Float) ++  "_i" ++ show embDim ++ "_h" ++ show hiddenSize ++ "_layer" ++ show numOfLayers ++ "/" ++ timeString

  createDirectoryIfMissing True newFolderPath

  when (not $ null usedBinFiles) $ do
    let binLogPath = newFolderPath ++ "/bin-files-used.txt"
    writeFile binLogPath (unlines usedBinFiles)
    putStrLn $ "Saved bin file list to " ++ binLogPath

  let modelFileName = newFolderPath ++ "/seq-class.model"
      frequentWordsFileName = newFolderPath ++ "/frequentWords.bin"
      graphFileName = newFolderPath ++ "/graph-seq-class.png"
      trainingTimeFileName = newFolderPath ++ "/training-time.txt"
      learningCurveTitle = "type: " ++ show delimiterToken ++ " bi: " ++ show biDirectional ++ " s: " ++ show numberOfBatch ++ " lr: " ++ show (asValue learningRate :: Float) ++ " i: " ++ show embDim ++ " h: " ++ show hiddenSize ++ " layer: " ++ show numOfLayers
      (losses, validLosses) = unzip lossesPair

  saveParams trainedModel modelFileName
  B.writeFile frequentWordsFileName (encode frequentWords')

  drawLearningCurve graphFileName learningCurveTitle [("training", reverse losses), ("validation", reverse validLosses)]

  evalResult <- evaluateModel device trainedModel testData biDirectional

  print $ erPredictions evalResult
  T.putStr $ TL.toStrict $ erClassificationReport evalResult

  let trainingTimeReport = TL.pack $ "Training Duration: " ++ show trainingDuration ++ "\n"
                                   ++ "Start Time: " ++ show startTime ++ "\n"
                                   ++ "End Time: " ++ show endTime ++ "\n"
  T.writeFile trainingTimeFileName (TL.toStrict trainingTimeReport)

  saveEvaluationReport newFolderPath evalResult allLabels

  print $ "isCorrects " ++ show (erCorrectFlags evalResult)
  print $ "Accuracy: " ++ show (erAccuracy evalResult)
