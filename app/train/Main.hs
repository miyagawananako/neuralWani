{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

import GHC.Generics                   --base
import qualified DTS.QueryTypes as QT
import Control.Monad (forM_)
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

forward :: Device -> Params -> ([Token], QT.DTTrule) -> (Token -> [Float]) -> IO Tensor
forward device model dataset oneHotTokens = do
  let input_original = map oneHotTokens $ fst dataset
  let input = map (\w -> (toDependent $ w_emb model) `matmul` (asTensor'' device w)) $ input_original
  let lstm = lstmLayers (lstmParams model)
  let dropout_prob = Nothing
  let (lstmOutput, (_, _)) = lstm dropout_prob (h0c0 model) $ (stack (Dim 0) input)
  print $ "lstmOutput " ++ show lstmOutput
  pure lstmOutput

predict :: Device -> Params -> ([Token], QT.DTTrule) -> (Token -> [Float]) -> (QT.DTTrule -> [Float]) -> IO Tensor
predict device model dataset oneHotTokens oneHotLabels = do
  let groundTruth = asTensor'' device (oneHotLabels $ snd dataset)
  let groundTruth' = argmax (Dim 0) KeepDim groundTruth
  -- let groundTruth' = case shape groundTruth of
  --       [n] -> reshape [1, n] groundTruth
  --       _   -> error $ "Unexpected shape: " ++ show (shape groundTruth)
  lstmOutput <- forward device model dataset oneHotTokens
  let mlp = linearLayer (mlpParams model)
  let output = mlp $ lstmOutput
  let shapeOutput = shape output
  print $ "shapeOutput " ++ show shapeOutput
  -- let output' = case shapeOutput of
  --       [1, n] -> softmax (Dim 0) (reshape [n] output)
  --       [_, n] -> softmax (Dim 0) (reshape [n] $ sliceDim 0 (length shapeOutput - 1) (length shapeOutput) 1 output)
  --       _      -> error $ "Unexpected shape: " ++ show shapeOutput
  -- let loss = binaryCrossEntropyLoss' groundTruth output'
  let output' = case shapeOutput of
        [1, n] -> logSoftmax (Dim 1) (reshape [1, n] output)
        [_, n] -> logSoftmax (Dim 1) (reshape [1, n] $ sliceDim 0 (length shapeOutput - 1) (length shapeOutput) 1 output)
        _      -> error $ "Unexpected shape: " ++ show shapeOutput
  -- let output' = logSoftmax (Dim 1) output  -- (バッチサイズ, クラス数)になるようにする
  print $ "groundTruth " ++ show groundTruth'
  print $ "output' " ++ show output'
  let loss = nllLoss' groundTruth' output'
  pure loss

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
      learningRate = 1e-3 :: Tensor
      graphFileName = "graph-seq-class.png"
      modelFileName = "seq-class.model"
  initModel <- sample hyperParams
  print $ "initModel " ++ show initModel
  ((trainedModel, _), losses) <- mapAccumM [1..iter] (initModel, GD) $ \epoc (model, opt) -> do
    loss <- predict device model (trainData !! 0) oneHotTokens oneHotLabels  -- 1データのみ
    -- loss <- predict device model ([Word1], QT.Var) oneHotTokens oneHotLabels
    let lossValue = (asValue loss) :: Float
    print $ "lossValue " ++ show lossValue
    showLoss 5 epoc lossValue
    print $ "loss " ++ show loss  -- Tensor Float []  8.1535
    u <- update model opt loss learningRate
    print $ "u " ++ show u  -- NaNが含まれている
    return (u, lossValue)
  -- 複数データ
  -- ((trainedModel), losses) <- mapAccumM [1..iter] (initModel) $ \epoc (model) -> do
  --   flip fix (0, model, trainData, 0) $ \loop (i, mdl, data_list, lastLossValue) -> do
  --     -- if i < length trainData then do
  --     --   loss <- predict device model (trainData !! i) oneHotTokens oneHotLabels  -- 1データのみ
  --     if length data_list > 0 then do
  --       let (oneData, restDataList) = splitAt 1 data_list
  --       print $ "oneData" ++ show oneData
  --       print $ "restDataList" ++ show (length restDataList)
  --       loss <- predict device mdl (head oneData) oneHotTokens oneHotLabels
  --       -- loss <- predict device model ([Word1, Word2], QT.Var) oneHotTokens oneHotLabels
  --       let lossValue = (asValue loss) :: Float
  --       print $ "lossValue " ++ show lossValue
  --       showLoss 5 epoc lossValue
  --       print $ "loss " ++ show loss  -- Tensor Float []  8.1535  -- 最終的にはここがNaNになって止まる
  --       u <- update model GD loss learningRate
  --       print $ "u " ++ show u  -- NaNが含まれている
  --       let (newModel, _) = u
  --       loop (i + 1, newModel, restDataList, lossValue)
  --     else return (mdl, lastLossValue)
  --   -- return (u, lossValue)

  print $ "losses " ++ show losses
  saveParams trainedModel modelFileName
  drawLearningCurve graphFileName "Learning Curve" [("", reverse losses)]