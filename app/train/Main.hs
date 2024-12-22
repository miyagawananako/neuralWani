{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

import GHC.Generics                   --base
import qualified DTS.QueryTypes as QT
import Control.Monad (forM_, forM)
import Data.Function(fix)
import System.Random.Shuffle (shuffleM)
import Data.Maybe
--hasktorch
import Torch.Tensor       (Tensor(..),asValue,reshape, shape, asTensor, asTensor', sliceDim)
import Torch.Device       (Device(..),DeviceType(..))
import Torch.Functional   (Dim(..),cat, softmax, matmul,nllLoss',argmax,KeepDim(..), transpose2D, binaryCrossEntropyLoss', stack, embedding', logSoftmax)
import Torch.NN           (Parameter,Parameterized,Randomizable,sample)
import Torch.Autograd     (IndependentTensor(..),makeIndependent)
import Torch.Optim        (GD(..))
import Torch.Train        (update,showLoss,saveParams,loadParams)
import Torch.Control      (mapAccumM)
import Torch.Tensor.TensorFactories (asTensor'',randnIO')
import Torch.TensorFactories (ones')
import Torch.Layer.Linear (LinearHypParams(..),LinearParams,linearLayer)
import Torch.Layer.LSTM   (LstmHypParams(..),LstmParams,lstmLayers)
import ML.Util.Dict    (sortWords,oneHotFactory) --nlp-tools
import ML.Exp.Chart   (drawLearningCurve) --nlp-tools
import ML.Exp.Classification (showClassificationReport) --nlp-tools
import SplitJudgment (Token(..), loadActionsFromBinary, getWordsFromJudgment, getFrequentWords, splitJudgment)

saveFilePath :: FilePath
saveFilePath = "data/proofSearchResult"

-- embed :: [Token] -> [Int]
-- embed = map fromEnum

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

instance Randomizable HypParams Params where
  sample HypParams{..} = do
    randomTensor1 <- randnIO' dev [num_layers, hidden_size]
    randomTensor2 <- randnIO' dev [num_layers, hidden_size]
    Params
      <$> sample (LstmHypParams dev bi_directional input_size hidden_size num_layers has_bias proj_size)
      <*> (makeIndependent =<< randnIO' dev [input_size, vocab_size])
      <*> sample (LinearHypParams dev has_bias hidden_size num_rules)
      <*> pure (0.01 * randomTensor1, 0.01 * randomTensor2)

forward :: Device -> Params -> ([Token], QT.DTTrule) -> (Token -> [Float]) -> IO (Tensor, (Tensor, Tensor))
forward device model dataset oneHotTokens = do
  let input_original = map oneHotTokens $ fst dataset
  let input = map (\w -> (toDependent $ w_emb model) `matmul` (asTensor'' device w)) $ input_original
  let lstm = lstmLayers (lstmParams model)
  let dropout_prob = Nothing
  -- print $ "h0c0 model " ++ show (h0c0 model)
  let (lstmOutput, newState) = lstm dropout_prob (h0c0 model) $ (stack (Dim 0) input)
  -- print $ "lstmOutput " ++ show lstmOutput
  pure (lstmOutput, newState)

predict :: Device -> Params -> ([Token], QT.DTTrule) -> (Token -> [Float]) -> (QT.DTTrule -> [Float]) -> IO (Tensor, Bool, (Tensor, Tensor))
predict device model dataset oneHotTokens oneHotLabels = do
  let groundTruth = asTensor'' device (oneHotLabels $ snd dataset)
  let groundTruth' = argmax (Dim 0) KeepDim groundTruth
  -- let groundTruth' = case shape groundTruth of
  --       [n] -> reshape [1, n] groundTruth
  --       _   -> error $ "Unexpected shape: " ++ show (shape groundTruth)
  (lstmOutput, newState) <- forward device model dataset oneHotTokens
  let mlp = linearLayer (mlpParams model)
  let output = mlp $ lstmOutput
  let shapeOutput = shape output
  -- print $ "shapeOutput " ++ show shapeOutput
  let y' = case shapeOutput of
        [1, n] -> softmax (Dim 0) (reshape [n] output)
        [_, n] -> softmax (Dim 0) (reshape [n] $ sliceDim 0 (length shapeOutput - 1) (length shapeOutput) 1 output)
        _      -> error $ "Unexpected shape: " ++ show shapeOutput
  -- print $ "y' " ++ show y'
  let output' = case shapeOutput of
        [1, n] -> logSoftmax (Dim 1) (reshape [1, n] output)
        [_, n] -> logSoftmax (Dim 1) (reshape [1, n] $ sliceDim 0 (length shapeOutput - 1) (length shapeOutput) 1 output)
        _      -> error $ "Unexpected shape: " ++ show shapeOutput
  -- let output' = logSoftmax (Dim 1) output  -- (バッチサイズ, クラス数)になるようにする
  -- print $ "groundTruth " ++ show groundTruth'
  -- print $ "output' " ++ show output'
  let loss = nllLoss' groundTruth' output'
  let classLabels = argmax (Dim 0) KeepDim y'
  let isCorrect = groundTruth' == classLabels
  pure (loss, isCorrect, newState)

main :: IO()
main = do
  dataset <- loadActionsFromBinary saveFilePath
  -- print dataset

  let wordList = concatMap (\(judgment, _) -> getWordsFromJudgment judgment) dataset
  -- print wordList
  let frequentWords = getFrequentWords wordList
  -- print frequentWords

  let constructorData = map (\(judgment, _) -> splitJudgment judgment frequentWords) dataset
  -- print constructorData
  -- let embeddedData = map (\judgment -> embed judgment) constructorData
  -- -- print embeddedData

  let ruleList = map (\(_, rule) -> rule) dataset
  -- print ruleList

  allData <- shuffleM $ zip constructorData ruleList
  let (trainData, restData) = splitAt (length allData * 7 `div` 10) allData
  let (validData, testData) = splitAt (length restData * 5 `div` 10) restData

  let iter = 10 :: Int
      device = Device CPU 0
      biDirectional = False
      input_size = 2
      numOfLayers = 1
      hiddenSize = 7
      has_bias = False
      (oneHotTokens, vocabSize) = oneHotFactory tokens  -- TODO: embedding'を使う
      proj_size = Nothing
      (oneHotLabels, numOfRules) = oneHotFactory labels
      hyperParams = HypParams device biDirectional input_size has_bias proj_size vocabSize numOfLayers hiddenSize numOfRules
      learningRate = 1e-5 :: Tensor
      graphFileName = "graph-seq-class.png"
      modelFileName = "seq-class.model"
  initModel <- sample hyperParams
  print $ "initModel " ++ show initModel
  -- ((trainedModel, _), losses) <- mapAccumM [1..iter] (initModel, GD) $ \epoc (model, opt) -> do
  --   (loss, _) <- predict device model (trainData !! 0) oneHotTokens oneHotLabels  -- 1データのみ
  --   -- loss <- predict device model ([Word1], QT.Var) oneHotTokens oneHotLabels
  --   let lossValue = (asValue loss) :: Float
  --   print $ "lossValue " ++ show lossValue
  --   showLoss 5 epoc lossValue
  --   print $ "loss " ++ show loss  -- Tensor Float []  8.1535
  --   u <- update model opt loss learningRate
  --   print $ "u " ++ show u  -- NaNが含まれている
  --   return (u, lossValue)
  -- 複数データ
  ((trainedModel), lossesPair) <- mapAccumM [1..iter] (initModel) $ \epoc (model) -> do
    flip fix (0, model, trainData, 0) $ \loop (i, mdl, data_list, sumLossValue) -> do
      -- if i < length trainData then do
      --   loss <- predict device model (trainData !! i) oneHotTokens oneHotLabels  -- 1データのみ
      if length data_list > 0 then do
        let (oneData, restDataList) = splitAt 1 data_list
        -- print $ "oneData" ++ show oneData
        -- print $ "restDataList" ++ show (length restDataList)
        (loss, _, newState) <- predict device mdl (head oneData) oneHotTokens oneHotLabels
        -- loss <- predict device model ([Word1, Word2], QT.Var) oneHotTokens oneHotLabels
        let lossValue = (asValue loss) :: Float
        -- print $ "lossValue " ++ show lossValue
        -- showLoss 5 epoc lossValue
        print $ "epoch " ++ show epoc  ++ " i " ++ show i ++ " loss " ++ show loss  -- Tensor Float []  8.1535
        let model' = mdl { h0c0 = newState }
        u <- update model' GD loss learningRate
        -- print $ "u " ++ show u  
        let (newModel, _) = u
        -- let updatedModel = newModel { h0c0 = newState }
        loop (i + 1, newModel, restDataList, sumLossValue + lossValue)
      else do
        validLosses <- forM validData $ \dataPoint -> do
          (loss, _, _) <- predict device model dataPoint oneHotTokens oneHotLabels
          let lossValue = (asValue loss) :: Float
          -- print $ "validation lossValue " ++ show lossValue
          return lossValue

        let avgTrainLoss = sumLossValue / fromIntegral (length trainData)

        let avgValidLoss = sum validLosses / fromIntegral (length validLosses)
        -- print $ "Average validation loss: " ++ show avgValidLoss

        return (mdl, (avgTrainLoss, avgValidLoss))
    -- return (u, lossValue)

  let (losses, validLosses) = unzip lossesPair
  -- print $ "losses " ++ show losses
  saveParams trainedModel modelFileName
  -- drawLearningCurve graphFileName "Learning Curve" [("", reverse losses)]
  drawLearningCurve graphFileName "Learning Curve" [("training", reverse losses), ("validation", reverse validLosses)]
    
  isCorrects <- forM testData $ \dataPoint -> do
    (_, isCorrect, _) <- predict device trainedModel dataPoint oneHotTokens oneHotLabels
    return isCorrect

  print $ "isCorrects " ++ show isCorrects

  let accuracy = fromIntegral (length (filter id isCorrects)) / fromIntegral (length isCorrects)
  print $ "Accuracy: " ++ show accuracy