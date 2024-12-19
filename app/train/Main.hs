{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

import GHC.Generics                   --base
import qualified DTS.QueryTypes as QT
--hasktorch
import Torch.Tensor       (Tensor(..),asValue,reshape, shape, asTensor)
import Torch.Device       (Device(..),DeviceType(..))
import Torch.Functional   (Dim(..),cat, softmax, matmul,nllLoss',argmax,KeepDim(..), transpose2D, binaryCrossEntropyLoss')
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
labels = [QT.Var, QT.Con, QT.Conv]

tokens :: [Token]
-- tokens = [minBound..]
tokens = [Word1,COMMA,Type',Word2,Word3]

-- 初期化のためのハイパーパラメータ
data HypParams = HypParams {
  dev :: Device,
  lstmHypParams :: LstmHypParams,
  wemb_dim :: Int
  } deriving (Eq, Show)

-- 学習されるパラメータmodelはこの型
data Params = Params {
  lstmParams :: LstmParams,
  w_emb :: Parameter, -- 再度埋め込む必要はないのでは？
  mlpParams :: LinearParams
  } deriving (Show, Generic)

instance Parameterized Params

-- data Paramsをここでつくる
instance Randomizable HypParams Params where
  sample HypParams{..} = do
    Params
      <$> sample lstmHypParams
      <*> (makeIndependent =<< randnIO' dev [hiddenSize lstmHypParams, wemb_dim])
      <*> sample (LinearHypParams dev (Torch.Layer.LSTM.hasBias lstmHypParams) (hiddenSize lstmHypParams) $ length labels)

oneHotLabel :: QT.DTTrule -> [Float]
oneHotLabel = ret
  where
    (ret, _) = oneHotFactory labels

oneHotToken :: Token -> [Float]
oneHotToken = ret
  where
    (ret, _) = oneHotFactory tokens

-- (7x12 and 14x1)でエラーが出ている。lstmLayerのなかで
-- lstmレイヤーが 初期の隠れ状態とセル状態のペアを引数に取る理由を考える
forward :: Device -> Params -> ([Token], QT.DTTrule) -> Int -> Int -> IO Tensor
forward device model dataset numOfLayers hiddenSize = do
  -- let input_original = asTensor'' device $ map tail $ map oneHotToken $ fst dataset  -- 形状は揃えるかも
  -- print input_original  -- Tensor Float [5,5] いい感じ
  let input_original = map tail $ map oneHotToken $ fst dataset  -- 形状は揃えるかも
  print input_original  -- Tensor Float [5,5] いい感じ
  -- let input = map (\w -> (toDependent $ w_emb model) `matmul` (asTensor'' device $ tail (oneHotToken w))) $ fst dataset
  let input = map (\w -> (toDependent $ w_emb model) `matmul` (asTensor'' device w)) $ input_original
  print input
  print $ cat (Dim 0) input
  print $ reshape [length input_original, hiddenSize] $ cat (Dim 0) input
  -- print $ cat (Dim 0) input
  -- ここまでは大丈夫
  let lstm = lstmLayers (lstmParams model)
  randomTensor <- randnIO' device [numOfLayers, hiddenSize] --- このサイズがおかしいかも(5はtokensの数)
  -- let (lstmOutput, (_, _)) = lstm Nothing (randomTensor, randomTensor) input
  let dropout_prob = Nothing
  let (lstmOutput, (_, _)) = lstm dropout_prob (randomTensor, randomTensor) $ (reshape [length input_original, hiddenSize] $ cat (Dim 0) input)
  print lstmOutput  --  出力されない!!!!!!!!
  pure lstmOutput

predict :: Device -> Params -> ([Token], QT.DTTrule) -> Int -> Int -> IO Tensor
predict device model dataset numOfLayers hiddenSize = do
  let groundTruth = asTensor'' device (tail (oneHotLabel $ snd dataset))
  lstmOutput <- forward device model dataset numOfLayers hiddenSize
  let mlp = linearLayer (mlpParams model)
  let lstmOutput_value = asValue lstmOutput :: [[Float]]
  let lastOutput = last lstmOutput_value
  let output = mlp $ (transpose2D $ reshape [length labels, 1] $ asTensor'' device lastOutput)
  let output' = softmax (Dim 0) (reshape [length labels] output)
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

  print $ length labels
  print $ length tokens

  print $ head trainData
  -- ([Word1,COMMA,Type',EOPre,EOPair,EOSig,Word1,EOPre,EOCon,Var'0,EOPre,EOTerm,Word1,EOPre,EOTyp],Var)

  -- input_sizeとwemb_dimが同じなのはいいのか
  let iter = 1 :: Int
      device = Device CUDA 0
      biDirectional = False
      input_size = length tokens
      -- lstm_dim = 32
      numOfLayers = 1
      hiddenSize = 7 --ゃダメな気がする）適当にかいてもforwardはすすんでしまう
      has_bias = True
      -- wemb_dim = length labels
      wemb_dim = length tokens
      -- (oneHotFor, wemb_dim) = oneHotFactory tokens
      proj_size = Nothing -- これがよくわからない
      hyperParams = HypParams device (LstmHypParams device biDirectional input_size hiddenSize numOfLayers has_bias proj_size) wemb_dim
      learningRate = 4e-3 :: Tensor
      graphFileName = "graph-seq-class.png"
      modelFileName = "seq-class.model"
  initModel <- sample hyperParams
  -- print initModel
  ((trainedModel, _), losses) <- mapAccumM [1..iter] (initModel, GD) $ \epoc (model, opt) -> do
    -- loss <- predict device model (trainData !! 0) numOfLayers  -- 1データのみ
    loss <- predict device model ([Word1,COMMA,Type',Word2,Word3,Word3], QT.Var) numOfLayers hiddenSize
    let lossValue = (asValue loss) :: Float
    print lossValue
    showLoss 5 epoc lossValue
    print loss  -- Tensor Float []  8.1535
    u <- update model opt loss learningRate
    print u  -- 出力されない
    return (u, lossValue)

  print losses
  saveParams trainedModel modelFileName
  drawLearningCurve graphFileName "Learning Curve" [("", reverse losses)]