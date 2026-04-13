{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad (foldM, when)
import Data.Maybe (mapMaybe)
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.ByteString as B
import Data.Store (encode)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Time as Time
import Data.Time.LocalTime (getZonedTime, zonedTimeToLocalTime)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Random.Shuffle (shuffleM)
import Prelude

import qualified DTS.Prover.Wani.BackwardRules as BR

import ML.Exp.Chart (drawLearningCurve)
import ML.Exp.Classification (showClassificationReport)
import Evaluate (EvaluationResult(..), saveEvaluationReport)
import SplitJudgment
  ( DelimiterToken (..),
    Token (..),
    buildWordMap,
    dttruleToRuleLabel,
    getConstantSymbolsFromJudgment,
    getFrequentConstantSymbols,
    splitJudgment,
  )
import Training.Shared (DatasetType(..), backwardRules, formationRules, countRule, splitByLabel, smoothData, parseDatasetType, loadDataset)

import qualified Torch.Tensor as D
import Torch.Typed
import qualified Prelude as P

allTokens :: [Token]
allTokens = [minBound ..]

allLabels :: [BR.RuleLabel]
allLabels = [minBound ..]

-- ===== Transformer model config (typed) =====

type NumLayers = 2
type NumHeads = 4
type FFNDim = 128
type PaddingIdx = 0

-- embedding vocabulary size for transformer input/output
-- 0 is reserved for padding
type NumEmbeds = 512

type EmbedDim = 128
type SeqLen = 64
type BatchSize = 16
type ModelDType = 'Float
type ModelDevice = '( 'CPU, 0)

type Model = TransformerLM NumLayers NumHeads FFNDim PaddingIdx NumEmbeds EmbedDim ModelDType ModelDevice
type Spec = TransformerLMSpec NumLayers NumHeads FFNDim PaddingIdx NumEmbeds EmbedDim ModelDType ModelDevice

modelSpec :: Spec
modelSpec =
  TransformerLMSpec @NumLayers @NumHeads @FFNDim @PaddingIdx @NumEmbeds @EmbedDim @ModelDType @ModelDevice
    (DropoutSpec 0.1)
    ( TransformerLayerSpec
        (MultiheadAttentionSpec (DropoutSpec 0.1))
        (DropoutSpec 0.1)
        1e-5
        (TransformerMLPSpec (DropoutSpec 0.1) (DropoutSpec 0.1) 1e-5)
    )

learningRateValue :: Float
learningRateValue = 5e-3

learningRate :: LearningRate ModelDevice ModelDType
learningRate = 5e-3

dropoutRate :: Float
dropoutRate = 0.1

numTokensValue :: Int
numTokensValue = P.length allTokens

numLabelsValue :: Int
numLabelsValue = P.length allLabels

seqLenValue :: Int
seqLenValue = natValI @SeqLen

tokenToId :: Token -> Int
tokenToId tok =
  case List.elemIndex tok allTokens of
    Just idx -> 1 + idx
    Nothing -> error $ "Unknown token: " ++ show tok

ruleToClassId :: BR.RuleLabel -> Int
ruleToClassId rule =
  case List.elemIndex rule allLabels of
    Just idx -> idx
    Nothing -> error $ "Unknown rule label: " ++ show rule

encodeInput :: [Token] -> [Int]
encodeInput tokens =
  let raw = map tokenToId tokens
      clipped = take seqLenValue raw
      padLen = P.max 0 (seqLenValue - P.length clipped)
    in clipped ++ P.replicate padLen 0

encodeTarget :: BR.RuleLabel -> [Int]
encodeTarget rule = P.replicate seqLenValue (ruleToClassId rule)

prepareTransformerRows :: [([Token], BR.RuleLabel)] -> [([Int], [Int], Int)]
prepareTransformerRows rows =
  map
    (\(tokens, rule) ->
       let tgt = ruleToClassId rule
        in (encodeInput tokens, encodeTarget rule, tgt)
    )
    rows

fullBatches :: Int -> [a] -> [[a]]
fullBatches batchSize = filter ((== batchSize) . P.length) . Split.chunksOf batchSize

padEvalBatch :: Int -> [a] -> [a]
padEvalBatch batchSize batchRows =
  case batchRows of
    [] -> []
    _ -> batchRows ++ P.replicate (P.max 0 (batchSize - P.length batchRows)) (P.last batchRows)

mkInputTensor :: [[Int]] -> Tensor ModelDevice 'Int64 '[BatchSize, SeqLen]
mkInputTensor rows = UnsafeMkTensor (D.asTensor rows)

mkTargetTensor :: [[Int]] -> Tensor ModelDevice 'Int64 '[BatchSize, SeqLen]
mkTargetTensor rows = UnsafeMkTensor (D.asTensor rows)

batchLoss ::
  Model ->
  Tensor ModelDevice 'Int64 '[BatchSize, SeqLen] ->
  Tensor ModelDevice 'Int64 '[BatchSize, SeqLen] ->
  Tensor ModelDevice ModelDType '[]
batchLoss model x y =
  let logits = forward model x
      logitsByClass = transpose @1 @2 logits
      logProbs = logSoftmax @1 logitsByClass
      classWeight = ones @'[NumEmbeds] @ModelDType @ModelDevice
   in nllLoss @'ReduceMean @BatchSize @NumEmbeds @'[SeqLen] classWeight (-100) logProbs y

classIdToRuleLabel :: Int -> Maybe BR.RuleLabel
classIdToRuleLabel classId
  | classId < 0 = Nothing
  | otherwise = Just (allLabels !! (classId `P.mod` P.length allLabels))

trainOneEpoch model optim batched = do
  (model', optim', losses) <- foldM step (model, optim, []) batched
  let meanLoss = if null losses then 0 else sum losses / fromIntegral (P.length losses)
  pure (model', optim', meanLoss)
  where
    step (mdl, opt, accLosses) batchRows = do
      let xs = map (\(x, _, _) -> x) batchRows
          ys = map (\(_, y, _) -> y) batchRows
          xTensor = mkInputTensor xs
          yTensor = mkTargetTensor ys
          loss = batchLoss mdl xTensor yTensor
          lossValue = D.asValue (toDynamic loss) :: Float
      (newModel, newOptim) <- runStep mdl opt loss learningRate
      pure (newModel, newOptim, lossValue : accLosses)

evaluateBatches :: Model -> [[([Int], [Int], Int)]] -> IO (Float, EvaluationResult)
evaluateBatches model batches = do
  stats <- mapM evalOne batches
  let losses = map (\t -> let (l, _, _) = t in l) stats
      batchPredictionsList = map (\t -> let (_, _, preds) = t in preds) stats
      predictions = concat batchPredictionsList
      accs = map (\t -> let (_, acc, _) = t in acc) stats
      meanLoss = if null losses then 0 else sum losses / fromIntegral (P.length losses)
      meanAcc = if null accs then 0 else sum accs / fromIntegral (P.length accs)
      correctFlags = map (uncurry (==)) predictions
      classReport = TL.fromStrict $ showClassificationReport (P.length allLabels) predictions
      evaluationResult = EvaluationResult
        { erPredictions = predictions
        , erCorrectFlags = correctFlags
        , erAccuracy = realToFrac meanAcc
        , erClassificationReport = classReport
        }
  pure (meanLoss, evaluationResult)
  where
    evalOne batchRows = do
      let actualCount = P.length batchRows
          paddedRows = padEvalBatch (natValI @BatchSize) batchRows
          xs = map (\(x, _, _) -> x) paddedRows
          ys = map (\(_, y, _) -> y) paddedRows
          xTensor = mkInputTensor xs
          yTensor = mkTargetTensor ys
          loss = batchLoss model xTensor yTensor
          lossValue = D.asValue (toDynamic loss) :: Float
          pred = argmax @2 @'DropDim (forward model xTensor)
          predVals = D.asValue (toDynamic pred) :: [[Int]]
          targetVals = D.asValue (toDynamic yTensor) :: [[Int]]
          paired = take actualCount (zip predVals targetVals)
          batchPredictions =
            mapMaybe
              (\(p, t) -> do
                  predLabel <- case p of
                    (x : _) -> classIdToRuleLabel x
                    _ -> Nothing
                  trueLabel <- case t of
                    (x : _) -> classIdToRuleLabel x
                    _ -> Nothing
                  pure (predLabel, trueLabel)
              )
              paired
          correct = P.length [() | (p, t) <- batchPredictions, p == t]
          acc = if null batchPredictions then 0 else fromIntegral correct / fromIntegral (P.length batchPredictions)
      pure (lossValue, acc, batchPredictions)

main :: IO ()
main = do
  args <- getArgs
  let delimiterToken = if null args then Unused else read (args !! 0) :: DelimiterToken
      datasetType = if P.length args < 2 then JSeM else parseDatasetType (args !! 1)
      iter = if P.length args < 3 then 20 else read (args !! 2) :: Int

  putStrLn $ "[Transformer] delimiterToken=" ++ show delimiterToken
  putStrLn $ "[Transformer] datasetType=" ++ show datasetType
  putStrLn $ "[Transformer] iter=" ++ show iter
  putStrLn $ "[Transformer] fixed batchSize=" ++ show (natValI @BatchSize) ++ " seqLen=" ++ show seqLenValue
  putStrLn $ "[Transformer] tokens=" ++ show numTokensValue ++ " labels=" ++ show numLabelsValue

  (originalDataset, usedBinFiles) <- loadDataset datasetType

  let isIncludeF = False
      isOnlyBackwardRules = True
      backwardDataset =
        if isOnlyBackwardRules
          then filter (\(_, rule) -> rule `elem` backwardRules) originalDataset
          else originalDataset
      filteredDataset =
        if isIncludeF
          then backwardDataset
          else filter (\(_, rule) -> rule `notElem` formationRules) backwardDataset
      dataset =
        mapMaybe
          (\(judgment, dttRule) ->
             case dttruleToRuleLabel dttRule of
               Just ruleLabel -> Just (judgment, ruleLabel)
               Nothing -> Nothing
          )
          filteredDataset
      wordList = concatMap (getConstantSymbolsFromJudgment . fst) dataset
      frequentWords = getFrequentConstantSymbols wordList
      wordMap = buildWordMap frequentWords
      tokenized = map (\(judgment, rule) -> (splitJudgment judgment wordMap delimiterToken, rule)) dataset
      ruleList = map snd tokenized

  print $ "countedRules " ++ show (countRule ruleList)

  splittedData <- splitByLabel tokenized
  (trainData, validData, testData) <- smoothData splittedData (Just 10)

  let countedTrainRules = countRule $ map snd trainData
  print $ "countedRules (training data) " ++ show countedTrainRules

  let trainRows = prepareTransformerRows trainData
      validRows = prepareTransformerRows validData
      testRows = prepareTransformerRows testData
      bsz = natValI @BatchSize
      validBatches = Split.chunksOf bsz validRows
      testBatches = Split.chunksOf bsz testRows

  putStrLn $ "train rows=" ++ show (P.length trainRows)

  startTime <- Time.getCurrentTime
  initModel <- sample modelSpec
  let initOptim = mkAdam 0 0.9 0.999 (flattenParameters initModel)

  (trainedModel, _finalOptim, lossPairs) <-
    foldM
      (\(mdl, opt, acc) epoch -> do
         shuffled <- shuffleM trainRows
         let trainBatches = fullBatches bsz shuffled
         (mdl', opt', trainLoss) <- trainOneEpoch mdl opt trainBatches
         (validLoss, validAcc) <- evaluateBatches mdl' validBatches
         putStrLn $
           "epoch " ++ show epoch ++
           " trainLoss=" ++ show trainLoss ++
           " validLoss=" ++ show validLoss ++
           " validAcc=" ++ show validAcc
         pure (mdl', opt', (trainLoss, validLoss) : acc)
      )
      (initModel, initOptim, [])
      ([1 .. iter] :: [Int])

  endTime <- Time.getCurrentTime
  let trainingDuration = Time.diffUTCTime endTime startTime
      (trainLosses, validLosses) = unzip (reverse lossPairs)

  (testLoss, testEvalResult) <- evaluateBatches trainedModel testBatches
  putStrLn $ "[Transformer] testLoss=" ++ show testLoss ++ " testAcc=" ++ show (erAccuracy testEvalResult)

  currentTime <- getZonedTime
  let timeString = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d_%H-%M-%S" (zonedTimeToLocalTime currentTime)
      datasetSuffix = case datasetType of
        JSeM -> "jsem"
        TPTP folder -> "tptp_" ++ folder
      newFolderPath =
        "trainedDataTransformer/"
          ++ datasetSuffix
          ++ "_type"
          ++ show delimiterToken
          ++ "_includeF"
          ++ show isIncludeF
          ++ "_backwardOnly"
          ++ show isOnlyBackwardRules
          ++ "_batch"
          ++ show bsz
          ++ "_seq"
          ++ show seqLenValue
          ++ "_iter"
          ++ show iter
          ++ "_lr"
          ++ show learningRateValue
          ++ "_layers"
          ++ show (natValI @NumLayers)
          ++ "_heads"
          ++ show (natValI @NumHeads)
          ++ "_ffn"
          ++ show (natValI @FFNDim)
          ++ "_embed"
          ++ show (natValI @EmbedDim)
          ++ "_embeds"
          ++ show (natValI @NumEmbeds)
          ++ "_pad"
          ++ show (natValI @PaddingIdx)
          ++ "_drop"
          ++ show dropoutRate
          ++ "_dtype"
          ++ show (dtypeVal @ModelDType)
          ++ "_device"
          ++ show (deviceVal @ModelDevice)
          ++ "_smooth10"
          ++ show True
          ++ "/"
          ++ timeString

  createDirectoryIfMissing True newFolderPath

  let modelFileName = newFolderPath ++ "/seq-class.model"
      frequentWordsFileName = newFolderPath ++ "/frequentWords.bin"
      trainingTimeFileName = newFolderPath ++ "/training-time.txt"
      reportFileName = newFolderPath ++ "/report.txt"

  writeFile modelFileName (show trainedModel)
  B.writeFile frequentWordsFileName (encode frequentWords)
  putStrLn $ "Saved model to " ++ modelFileName
  putStrLn $ "Saved frequentWords to " ++ frequentWordsFileName

  when (not $ null usedBinFiles) $ do
    let binLogPath = newFolderPath ++ "/bin-files-used.txt"
    writeFile binLogPath (unlines usedBinFiles)
    putStrLn $ "Saved bin file list to " ++ binLogPath

  let graphFileName = newFolderPath ++ "/graph-transformer.png"
      reportFileName = newFolderPath ++ "/report.txt"
      learningCurveTitle =
        "Transformer type=" ++ show delimiterToken ++
        " batch=" ++ show bsz ++
        " seq=" ++ show seqLenValue ++
        " iter=" ++ show iter

  drawLearningCurve graphFileName learningCurveTitle [("training", trainLosses), ("validation", validLosses)]

  saveEvaluationReport newFolderPath testEvalResult allLabels
  putStrLn $ "Saved evaluation report to " ++ newFolderPath

  let report =
        TL.pack $
          "Training Duration: " ++ show trainingDuration ++ "\n"
            ++ "Start Time: " ++ show startTime ++ "\n"
            ++ "End Time: " ++ show endTime ++ "\n"
            ++ "Test Loss: " ++ show testLoss ++ "\n"
            ++ "Test Accuracy (first token class): " ++ show (erAccuracy testEvalResult) ++ "\n"
  T.writeFile trainingTimeFileName (TL.toStrict report)
  T.writeFile reportFileName (TL.toStrict report)

  putStrLn $ "Saved graph to " ++ graphFileName
  putStrLn $ "Saved training time to " ++ trainingTimeFileName
  putStrLn $ "Saved report to " ++ reportFileName
