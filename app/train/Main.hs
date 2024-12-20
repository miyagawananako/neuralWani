{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

import GHC.Generics                   --base
import qualified DTS.QueryTypes as QT
--hasktorch
import Torch.Tensor       (Tensor(..),asValue,reshape, shape, asTensor)
import Torch.Device       (Device(..),DeviceType(..))
import Torch.Functional   (Dim(..),cat, softmax, matmul,nllLoss',argmax,KeepDim(..), transpose2D, binaryCrossEntropyLoss', stack, embedding')
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

labels :: [QT.DTTrule]
-- labels = [minBound..]
labels = [QT.Var, QT.Con]

tokens :: [Token]
-- tokens = [minBound..]
tokens = [Word1,COMMA,Type',Word2]

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
      <*> pure (randomTensor1, randomTensor2)

forward :: Device -> Params -> ([Token], QT.DTTrule) -> (Token -> [Float]) -> IO Tensor
forward device model dataset oneHotTokens = do
  let input_original = map oneHotTokens $ fst dataset
  let input = map (\w -> (toDependent $ w_emb model) `matmul` (asTensor'' device w)) $ input_original
  let lstm = lstmLayers (lstmParams model)
  let dropout_prob = Nothing
  let (lstmOutput, (_, _)) = lstm dropout_prob (h0c0 model) $ (stack (Dim 0) input)
  pure lstmOutput

predict :: Device -> Params -> ([Token], QT.DTTrule) -> (Token -> [Float]) -> (QT.DTTrule -> [Float]) -> IO Tensor
predict device model dataset oneHotTokens oneHotLabels = do
  let groundTruth = asTensor'' device (oneHotLabels $ snd dataset)
  lstmOutput <- forward device model dataset oneHotTokens
  let mlp = linearLayer (mlpParams model)
  let output = mlp $ lstmOutput
  let shapeOutput = shape output
  let output' = case shapeOutput of
        [_, n] -> softmax (Dim 0) (reshape [n] output)
        _      -> error $ "Unexpected shape: " ++ show shapeOutput
  let loss = binaryCrossEntropyLoss' output' groundTruth
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

  let trainData = zip constructorData ruleList
  print $ length trainData

  print $ length labels + 1
  print $ length tokens + 1

  print $ head trainData
  -- ([Word1,COMMA,Type',EOPre,EOPair,EOSig,Word1,EOPre,EOCon,Var'0,EOPre,EOTerm,Word1,EOPre,EOTyp],Var)

  let iter = 1 :: Int
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
      learningRate = 4e-3 :: Tensor
      graphFileName = "graph-seq-class.png"
      modelFileName = "seq-class.model"
  initModel <- sample hyperParams
  -- print initModel
  ((trainedModel, _), losses) <- mapAccumM [1..iter] (initModel, GD) $ \epoc (model, opt) -> do
    -- loss <- predict device model (trainData !! 0) numOfLayers  -- 1データのみ
    loss <- predict device model ([Word1], QT.Var) oneHotTokens oneHotLabels
    let lossValue = (asValue loss) :: Float
    print lossValue
    showLoss 5 epoc lossValue
    print loss  -- Tensor Float []  8.1535
    u <- update model opt loss learningRate
    print u  -- NaNが含まれている
    return (u, lossValue)

  print losses
  saveParams trainedModel modelFileName
  drawLearningCurve graphFileName "Learning Curve" [("", reverse losses)]