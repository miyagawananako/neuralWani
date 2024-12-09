{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

import GHC.Generics                   --base
import qualified DTS.QueryTypes as QT
import qualified DTS.DTTdeBruijn as U
import qualified Data.ByteString as B --bytestring
import qualified Data.Text.Lazy as T  --text
import Data.Store (decode)
import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Data.Ord (Down(..))
import qualified Data.Set as Set
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

saveFilePath :: FilePath
saveFilePath = "data/proofSearchResult"

loadActionsFromBinary :: FilePath -> IO [(U.Judgment, QT.DTTrule)]
loadActionsFromBinary filepath = do
  binary <- B.readFile filepath
  case decode binary of
    Left peek_exception -> error $ "Could not parse dic file " ++ filepath ++ ": " ++ (show peek_exception)
    Right actions -> return actions

getWordsFromPreterm :: U.Preterm -> [T.Text]
getWordsFromPreterm preterm = case preterm of
  U.Con c  -> [c]
  U.Pi a b -> getWordsFromPreterm a ++ getWordsFromPreterm b
  U.Lam m  -> getWordsFromPreterm m
  U.App m n -> getWordsFromPreterm m ++ getWordsFromPreterm n
  U.Not m  -> getWordsFromPreterm m
  U.Sigma a b  -> getWordsFromPreterm a ++ getWordsFromPreterm b
  U.Pair m n   -> getWordsFromPreterm m ++ getWordsFromPreterm n
  U.Proj _ m   -> getWordsFromPreterm m
  U.Disj a b   -> getWordsFromPreterm a ++ getWordsFromPreterm b
  U.Iota _ m   -> getWordsFromPreterm m
  U.Unpack p h m n -> getWordsFromPreterm p ++ getWordsFromPreterm h ++ getWordsFromPreterm m ++ getWordsFromPreterm n
  U.Succ n     -> getWordsFromPreterm n
  U.Natrec n e f -> getWordsFromPreterm n ++ getWordsFromPreterm e ++ getWordsFromPreterm f
  U.Eq a m n   -> getWordsFromPreterm a ++ getWordsFromPreterm m ++ getWordsFromPreterm n
  U.Refl a m   -> getWordsFromPreterm a ++ getWordsFromPreterm m
  U.Idpeel m n -> getWordsFromPreterm m ++ getWordsFromPreterm n
  _ -> []

getWordsFromPreterms :: [U.Preterm] -> [T.Text]
getWordsFromPreterms preterms = concatMap (\preterm -> getWordsFromPreterm preterm) preterms

getWordsFromSignature :: U.Signature -> [T.Text]
getWordsFromSignature signature = concatMap (\(name, preterm) -> [name] ++ getWordsFromPreterm preterm) signature

allowDuplicateWords :: Bool
allowDuplicateWords = True

getWordsFromJudgment :: U.Judgment -> [T.Text]
getWordsFromJudgment judgment =
  if allowDuplicateWords then wordList
  else Set.toList . Set.fromList $ wordList
  where
    wordList =
      getWordsFromSignature (U.signtr judgment) ++
      getWordsFromPreterms (U.contxt judgment) ++
      getWordsFromPreterm (U.trm judgment) ++
      getWordsFromPreterm (U.typ judgment)

getFrequentWords :: [T.Text] -> [T.Text]
getFrequentWords frequentWords = take 31 $ map fst $ sortOn (Down . snd) $ Map.toList wordFreqMap
  where
    wordFreqMap :: Map.Map T.Text Int
    wordFreqMap = foldr (\word acc -> Map.insertWith (+) word 1 acc) Map.empty frequentWords

data Token =  FST | SND | COMMA | EOPair | EOPre | EOSig | EOCon | EOTerm | EOTyp | LPAREN | RPAREN | SEP
            | Word1 | Word2 | Word3 | Word4 | Word5 | Word6 | Word7 | Word8 | Word9 | Word10 | Word11 | Word12 | Word13 | Word14 | Word15 | Word16 | Word17 | Word18 | Word19 | Word20 | Word21 | Word22 | Word23 | Word24 | Word25 | Word26 | Word27 | Word28 | Word29 | Word30 | Word31 | UNKNOWN
            | Var'0 | Var'1 | Var'2 | Var'3 | Var'4 | Var'5 | Var'6 | Var'unknown
            | Type' | Kind' | Pi' | Lam' | App' | Not' | Sigma' | Pair' | Proj' | Disj' | Iota' | Unpack' | Bot' | Unit' | Top' | Entity' | Nat' | Zero' | Succ' | Natrec' | Eq' | Refl' | Idpeel'
  deriving (Enum, Show, Bounded, Eq, Ord)

isParen :: Bool
isParen = False

isSep :: Bool
isSep = False

textToToken :: T.Text -> [T.Text] -> [Token]
textToToken text frequentWords =
  case () of
    _ | length frequentWords > 0 && text == frequentWords !! 0 -> [Word1]
      | length frequentWords > 1 && text == frequentWords !! 1 -> [Word2]
      | length frequentWords > 2 && text == frequentWords !! 2 -> [Word3]
      | length frequentWords > 3 && text == frequentWords !! 3 -> [Word4]
      | length frequentWords > 4 && text == frequentWords !! 4 -> [Word5]
      | length frequentWords > 5 && text == frequentWords !! 5 -> [Word6]
      | length frequentWords > 6 && text == frequentWords !! 6 -> [Word7]
      | length frequentWords > 7 && text == frequentWords !! 7 -> [Word8]
      | length frequentWords > 8 && text == frequentWords !! 8 -> [Word9]
      | length frequentWords > 9 && text == frequentWords !! 9 -> [Word10]
      | length frequentWords > 10 && text == frequentWords !! 10 -> [Word11]
      | length frequentWords > 11 && text == frequentWords !! 11 -> [Word12]
      | length frequentWords > 12 && text == frequentWords !! 12 -> [Word13]
      | length frequentWords > 13 && text == frequentWords !! 13 -> [Word14]
      | length frequentWords > 14 && text == frequentWords !! 14 -> [Word15]
      | length frequentWords > 15 && text == frequentWords !! 15 -> [Word16]
      | length frequentWords > 16 && text == frequentWords !! 16 -> [Word17]
      | length frequentWords > 17 && text == frequentWords !! 17 -> [Word18]
      | length frequentWords > 18 && text == frequentWords !! 18 -> [Word19]
      | length frequentWords > 19 && text == frequentWords !! 19 -> [Word20]
      | length frequentWords > 20 && text == frequentWords !! 20 -> [Word21]
      | length frequentWords > 21 && text == frequentWords !! 21 -> [Word22]
      | length frequentWords > 22 && text == frequentWords !! 22 -> [Word23]
      | length frequentWords > 23 && text == frequentWords !! 23 -> [Word24]
      | length frequentWords > 24 && text == frequentWords !! 24 -> [Word25]
      | length frequentWords > 25 && text == frequentWords !! 25 -> [Word26]
      | length frequentWords > 26 && text == frequentWords !! 26 -> [Word27]
      | length frequentWords > 27 && text == frequentWords !! 27 -> [Word28]
      | length frequentWords > 28 && text == frequentWords !! 28 -> [Word29]
      | length frequentWords > 29 && text == frequentWords !! 29 -> [Word30]
      | length frequentWords > 30 && text == frequentWords !! 30 -> [Word31]
      | otherwise -> [UNKNOWN]

varToToken :: Int -> [Token]
varToToken i =
  case i of
    0 -> [Var'0]
    1 -> [Var'1]
    2 -> [Var'2]
    3 -> [Var'3]
    4 -> [Var'4]
    5 -> [Var'5]
    6 -> [Var'6]
    _ -> [Var'unknown]

selectorToToken :: U.Selector -> [Token]
selectorToToken s = case s of
  U.Fst -> [FST]
  U.Snd -> [SND]

wrapPreterm :: [Token] -> [Token]
wrapPreterm xs =
  if isParen then [LPAREN] ++ xs ++ [RPAREN]
  else if isSep then xs ++ [SEP] else xs ++ [EOPre]

splitPreterm :: U.Preterm -> [T.Text] -> [Token]
splitPreterm preterm frequentWords = case preterm of
  U.Var i -> wrapPreterm (varToToken i)
  U.Con c -> wrapPreterm (textToToken c frequentWords)
  U.Type -> wrapPreterm [Type']
  U.Kind -> wrapPreterm [Kind']
  U.Pi a b -> wrapPreterm ([Pi'] ++ splitPreterm a frequentWords ++ splitPreterm b frequentWords)
  U.Lam m -> wrapPreterm ([Lam'] ++ splitPreterm m frequentWords)
  U.App m n -> wrapPreterm ([App'] ++ splitPreterm m frequentWords ++ splitPreterm n frequentWords)
  U.Not m -> wrapPreterm ([Not'] ++ splitPreterm m frequentWords)
  U.Sigma a b -> wrapPreterm ([Sigma'] ++ splitPreterm a frequentWords ++ splitPreterm b frequentWords)
  U.Pair m n -> wrapPreterm ([Pair'] ++ splitPreterm m frequentWords ++ splitPreterm n frequentWords)
  U.Proj s m -> wrapPreterm ([Proj'] ++ selectorToToken s ++ splitPreterm m frequentWords)
  U.Disj a b -> wrapPreterm ([Disj'] ++ splitPreterm a frequentWords ++ splitPreterm b frequentWords)
  U.Iota s m -> wrapPreterm ([Iota'] ++ selectorToToken s ++ splitPreterm m frequentWords)
  U.Unpack p h m n -> wrapPreterm ([Unpack'] ++ splitPreterm p frequentWords ++ splitPreterm h frequentWords ++ splitPreterm m frequentWords ++ splitPreterm n frequentWords)
  U.Bot -> wrapPreterm [Bot']
  U.Unit -> wrapPreterm [Unit']
  U.Top -> wrapPreterm [Top']
  U.Entity -> wrapPreterm [Entity']
  U.Nat -> wrapPreterm [Nat']
  U.Zero -> wrapPreterm [Zero']
  U.Succ m -> wrapPreterm ([Succ'] ++ splitPreterm m frequentWords)
  U.Natrec n e f -> wrapPreterm ([Natrec'] ++ splitPreterm n frequentWords ++ splitPreterm e frequentWords ++ splitPreterm f frequentWords)
  U.Eq a m n -> wrapPreterm ([Eq'] ++ splitPreterm a frequentWords ++ splitPreterm m frequentWords ++ splitPreterm n frequentWords)
  U.Refl a m -> wrapPreterm ([Refl'] ++ splitPreterm a frequentWords ++ splitPreterm m frequentWords)
  U.Idpeel m n -> wrapPreterm ([Idpeel'] ++ splitPreterm m frequentWords ++ splitPreterm n frequentWords)

splitPreterms:: [U.Preterm] -> [T.Text]-> [Token]
splitPreterms preterms frequentWords = concatMap (\preterm -> splitPreterm preterm frequentWords) preterms

wrapPair :: [Token] -> [Token]
wrapPair xs =
  if isParen then [LPAREN] ++ xs ++ [RPAREN]
  else if isSep then xs ++ [SEP] else xs ++ [EOPair]

splitSignature :: U.Signature -> [T.Text] -> [Token]
splitSignature signature frequentWords = concatMap (\(name, preterm) -> wrapPair (textToToken name frequentWords ++ [COMMA] ++ splitPreterm preterm frequentWords)) signature

wrapSignature :: [Token] -> [Token]
wrapSignature xs =
  if isParen then [LPAREN] ++ xs ++ [RPAREN]
  else if isSep then xs ++ [SEP] else xs ++ [EOSig]

wrapContext :: [Token] -> [Token]
wrapContext xs =
  if isParen then [LPAREN] ++ xs ++ [RPAREN]
  else if isSep then xs ++ [SEP] else xs ++ [EOCon]

wrapTerm ::[Token] -> [Token]
wrapTerm xs =
  if isParen then [LPAREN] ++ xs ++ [RPAREN]
  else if isSep then xs ++ [SEP] else xs ++ [EOTerm]

wrapTyp :: [Token] -> [Token]
wrapTyp xs =
  if isParen then [LPAREN] ++ xs ++ [RPAREN]
  else if isSep then xs else xs ++ [EOTyp]

splitJudgment :: U.Judgment -> [T.Text] -> [Token]
splitJudgment judgment frequentWords =
  wrapSignature (splitSignature (U.signtr judgment) frequentWords) ++
  wrapContext (splitPreterms (U.contxt judgment) frequentWords) ++
  wrapTerm (splitPreterm (U.trm judgment) frequentWords) ++
  wrapTyp (splitPreterm (U.typ judgment) frequentWords)

-- embed :: [Token] -> [Int]
-- embed = map fromEnum

labels :: [QT.DTTrule]
labels = [minBound..]

tokens :: [Token]
tokens = [minBound..]

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

instance Randomizable HypParams Params where
  sample HypParams{..} = do
    Params
      <$> sample lstmHypParams
      <*> (makeIndependent =<< randnIO' dev [hiddenSize lstmHypParams, wemb_dim])
      <*> sample (LinearHypParams dev (Torch.Layer.LSTM.hasBias lstmHypParams) (hiddenSize lstmHypParams) $ length labels)

-- lstmレイヤーが 初期の隠れ状態とセル状態のペアを引数に取る理由を考える
forward :: Device -> Params -> ([Token], QT.DTTrule) -> IO Tensor
forward device model dataset = do
  let (oneHotToken, _) = oneHotFactory tokens  -- 固定値なので別のところにおきたい
  let input = asTensor'' device $ map tail $ map oneHotToken $ fst dataset  -- 形状は揃えるかも
  -- print input  -- Tensor Float [14,76]
  let lstm = lstmLayers (lstmParams model)
  randomTensor <- randnIO' device [2, length labels]
  -- print randomTensor
  let (lstmOutput, (_, _)) = lstm Nothing (randomTensor, randomTensor) input
  -- print lstmOutput
  pure lstmOutput

predict :: Device -> Params -> ([Token], QT.DTTrule) -> IO Tensor
predict device model dataset = do
  let (oneHotLabels, _) = oneHotFactory labels  -- 固定値なので別のところにおきたい
  let groundTruth = asTensor'' device (tail (oneHotLabels $ snd dataset))
  -- print groundTruth
  lstmOutput <- forward device model dataset
  let mlp = linearLayer (mlpParams model)
  let lstmOutput_value = asValue lstmOutput :: [[Float]]
  -- print lstmOutput_value
  let lastOutput = last lstmOutput_value
  -- print lastOutput
  -- print $ length lastOutput
  -- print $ (reshape [length labels, 1] $ asTensor'' device lastOutput)  -- Tensor Float [23]
  let output = mlp $ (transpose2D $ reshape [length labels, 1] $ asTensor'' device lastOutput)
  -- print output
  -- print $ reshape [length labels] output
  let output' = softmax (Dim 0) (reshape [length labels] output)-- Tensor Float [23]
  -- print output'
  -- print groundTruth
  let loss = binaryCrossEntropyLoss' output' groundTruth  -- Prelude.!!: index too largeのエラーが出る→直した
  pure loss

-- 上記の関数での以下のエラーを解消する→出なくなった
-- Exception: Differentiated tensor has more than a single element; type: std::runtime_error
-- train-exe: CppStdException e "Differentiated tensor has more than a single element"(Just "std::runtime_error")

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

  let iter = 10 :: Int
      device = Device CPU 0
      input_size = length tokens
      -- lstm_dim = 32
      numOfLayers = 2
      wemb_dim = length labels  -- hiddenSize（これじゃダメな気がする）
      proj_size = Nothing -- これがよくわからない
      hyperParams = HypParams device (LstmHypParams device False input_size wemb_dim numOfLayers True proj_size) wemb_dim  -- 合っているか怪しいwemb_dimを使いすぎている
      learningRate = 4e-3 :: Tensor
      graphFileName = "graph-seq-class.png"
      modelFileName = "seq-class.model"
  initModel <- sample hyperParams
  -- print initModel
  ((trainedModel, _), losses) <- mapAccumM [1..iter] (initModel, GD) $ \epoc (model, opt) -> do
    batchLoss <- predict device model (trainData !! 0)  -- 1データのみ
    let lossValue = (asValue batchLoss) :: Float
    print lossValue
    showLoss 5 epoc lossValue
    print batchLoss  -- Tensor Float []  8.1535 
    u <- update model opt batchLoss learningRate
    print u  -- 出力されない
    return (u, lossValue)

  print losses
  saveParams trainedModel modelFileName
  drawLearningCurve graphFileName "Learning Curve" [("", reverse losses)]