{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

import GHC.Generics                   --base
import Control.Monad (forM)
import System.Random.Shuffle (shuffleM)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import qualified Data.Text.IO as T    --text
import Data.Time.LocalTime
import qualified Data.Time as Time
import qualified Data.ByteString as B --bytestring
import qualified Data.Text.Encoding as E
import Data.Ord (Down(..))
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.List.Split as List
import           System.Environment (getArgs)
import System.Mem (performGC)
import qualified DTS.QueryTypes as QT

--hasktorch関連のインポート
import Torch.Tensor       (Tensor(..),asValue,reshape, shape, asTensor, sliceDim, toDevice)
import Torch.Device       (Device(..),DeviceType(..))
import Torch.Functional   (Dim(..),nllLoss',argmax,KeepDim(..), embedding', logSoftmax)
import Torch.NN           (Parameter,Parameterized,Randomizable,sample, flattenParameters)
import Torch.Autograd     (IndependentTensor(..),makeIndependent)
import Torch.Optim        (mkAdam)
import Torch.Train        (update,saveParams,loadParams)
import Torch.Control      (mapAccumM)
import Torch.Tensor.TensorFactories (randnIO')
import Torch.Layer.Linear (LinearHypParams(..),LinearParams,linearLayer)
import Torch.Layer.LSTM   (LstmHypParams(..),LstmParams,lstmLayers)

--可視化と評価用のツール
import ML.Exp.Chart   (drawLearningCurve, drawConfusionMatrix) --nlp-tools
import ML.Exp.Classification (showClassificationReport) --nlp-tools

--プロジェクト固有のモジュール
import SplitJudgment (Token(..), loadActionsFromBinary, getConstantSymbolsFromJudgment, getFrequentConstantSymbols, splitJudgment, DelimiterToken(..))

-- | 証明探索結果のファイルパス
proofSearchResultFilePath :: FilePath
proofSearchResultFilePath = "data/proofSearchResult"

-- | すべてのラベル（DTT規則）のリスト
allLabels :: [QT.DTTrule]
allLabels = [minBound..]

-- | すべてのトークンのリスト
allTokens :: [Token]
allTokens = [minBound..]

-- | ニューラルネットワークのハイパーパラメータを定義するデータ型
data HypParams = HypParams {
  dev :: Device,           -- ^ 使用するデバイス
  bi_directional :: Bool,  -- ^ 双方向LSTMを使用するかどうか
  emb_dim :: Int,          -- ^ 埋め込み層の次元数
  has_bias :: Bool,        -- ^ バイアスを使用するかどうか
  proj_size :: Maybe Int,  -- ^ 投影サイズ（オプション）
  vocab_size :: Int,       -- ^ 語彙サイズ
  num_layers :: Int,       -- ^ LSTMの層数
  hidden_size :: Int,      -- ^ 隠れ層のサイズ
  num_rules :: Int         -- ^ 規則の数
  } deriving (Eq, Show)

-- | ニューラルネットワークのパラメータを定義するデータ型
data Params = Params {
  lstm_params :: LstmParams,  -- ^ LSTM層のパラメータ
  w_emb :: Parameter,         -- ^ 埋め込み層のパラメータ
  mlp_params :: LinearParams, -- ^ 全結合層のパラメータ
  hc :: (Tensor, Tensor)      -- ^ 隠れ状態とセル状態の初期値
  } deriving (Show, Generic)

instance Parameterized Params

-- | Paramsを初期化するためのRandomizableインスタンス
-- ハイパーパラメータに基づいてモデルのパラメータをランダムに初期化します
instance Randomizable HypParams Params where
  sample HypParams{..} = do
    -- 双方向LSTMの場合は隠れ層の次元数を2倍にする
    let d = if bi_directional then 2 else 1
    -- 隠れ状態とセル状態の初期値を生成
    randomTensor1 <- randnIO' dev [d * num_layers, hidden_size]
    randomTensor2 <- randnIO' dev [d * num_layers, hidden_size]
    Params
      <$> sample (LstmHypParams dev bi_directional emb_dim hidden_size num_layers has_bias proj_size)
      <*> (makeIndependent =<< randnIO' dev [vocab_size, emb_dim])
      <*> sample (LinearHypParams dev has_bias (d * hidden_size) num_rules)
      <*> pure (0.01 * randomTensor1, 0.01 * randomTensor2)

-- | LSTMモデルの順伝播（forward）関数
-- 入力データから予測を生成します
--
-- 引数：
-- * device - 使用するデバイス（CPU/GPU）
-- * params - モデルのパラメータ
-- * dataset - 入力データ（トークン列と規則）
-- * bi_directional - 双方向LSTMを使用するかどうか
--
-- 戻り値：
-- * 各規則の予測確率（対数ソフトマックス適用済み）
forward :: Device -> Params -> ([Token], QT.DTTrule) -> Bool -> IO Tensor
forward device Params{..} dataset bi_directional = do
  -- トークンをインデックスに変換し、テンソルに変換
  let inputIndices = map fromEnum $ fst dataset
      idxs = toDevice device $ asTensor (inputIndices :: [Int])
      -- 埋め込み層を適用
      input = embedding' (toDependent w_emb) idxs
      dropout_prob = Nothing
      -- LSTM層を適用
      (_, (h, _)) = lstmLayers lstm_params dropout_prob False hc $ input
  -- 最後の層の出力を取得（双方向の場合は両方向の出力を結合）
  lastOutput <- extractLastOutput h bi_directional
  -- 全結合層とソフトマックスを適用
  let output = linearLayer mlp_params lastOutput
      output' = logSoftmax (Dim 1) output
  pure output'

-- | LSTMの最後の層の出力を抽出する関数
-- 双方向LSTMの場合は両方向の出力を結合します
--
-- 引数：
-- * tensor - LSTMの出力テンソル
-- * bi_directional - 双方向LSTMを使用するかどうか
--
-- 戻り値：
-- * 最後の層の出力（双方向の場合は結合された出力）
extractLastOutput :: Tensor -> Bool -> IO Tensor
extractLastOutput tensor bi_directional = do
  let shapeInput = shape tensor
  case bi_directional of
    True -> do
      -- 双方向の場合、最後の2つの出力を取得して結合
      let lastOutput1 = sliceDim 0 (shapeInput !! 0 - 2) (shapeInput !! 0) 1 tensor  -- [2, hidden_size]
      return $ reshape [1, 2 * (shapeInput !! (length shapeInput - 1))] lastOutput1  -- [1, 2 * hidden_size]
    False -> do
      -- 単方向の場合、最後の出力を取得
      case shapeInput of
        [1, _] -> return tensor
        [_, _] -> return $ sliceDim 0 (shapeInput !! 0 - 1) (shapeInput !! 0) 1 tensor
        _      -> error $ "Unexpected shape: " ++ show shapeInput

-- | 規則の出現回数をカウントする関数
-- 各規則の出現頻度を計算し、頻度の降順でソートして返します
--
-- 引数：
-- * rules - 規則のリスト
--
-- 戻り値：
-- * (規則, 出現回数)のペアのリスト（出現回数の降順）
countRule :: [QT.DTTrule] -> [(QT.DTTrule, Int)]
countRule rules = List.sortOn (Down . snd) $ Map.toList ruleFreqMap
  where
    ruleFreqMap :: Map.Map QT.DTTrule Int
    ruleFreqMap = foldr (\word acc -> Map.insertWith (+) word 1 acc) Map.empty rules

-- | データセットをラベル（規則）ごとに分割する関数
-- 各規則に対応するデータをグループ化します
--
-- 引数：
-- * dataset - (トークン列, 規則)のペアのリスト
--
-- 戻り値：
-- * (規則, その規則に対応するデータのリスト)のペアのリスト
splitByLabel :: [([Token], QT.DTTrule)] -> IO [(QT.DTTrule, [([Token], QT.DTTrule)])]
splitByLabel dataset = return $ splitByLabel' dataset Map.empty
  where
    splitByLabel' [] acc = Map.toList acc
    splitByLabel' ((tokens', rule):xs) acc =
      let data' = (tokens', rule)
          acc' = Map.insertWith (++) rule [data'] acc
      in splitByLabel' xs acc'

-- | データセットを訓練・検証・テストに分割する関数
-- 各規則のデータを適切な比率で分割します
--
-- 引数：
-- * splittedData - splitByLabelで分割されたデータ
-- * threshold - 各規則のデータ数の上限（オプション）
--
-- 戻り値：
-- * (訓練データ, 検証データ, テストデータ)のタプル
-- * 訓練：検証：テスト = 8：1：1 の比率で分割
smoothData :: [(QT.DTTrule, [([Token], QT.DTTrule)])] -> Maybe Int -> IO ([([Token], QT.DTTrule)], [([Token], QT.DTTrule)], [([Token], QT.DTTrule)])
smoothData splittedData threshold = smoothData' splittedData [] [] []
  where
    smoothData' [] trainDataAcc validDataAcc testDataAcc = return (trainDataAcc, validDataAcc, testDataAcc)
    smoothData' ((_, dataList):remainingData) trainDataAcc validDataAcc testDataAcc = do
      -- データをシャッフル
      shuffledData <- shuffleM dataList
      -- 必要に応じてデータ数を制限
      let limitedData = case threshold of
            Nothing -> shuffledData
            Just threshold' -> take threshold' shuffledData
          -- 8:2の比率で訓練データとその他に分割
          (trainData, restData) = splitAt (length limitedData * 8 `div` 10) limitedData
          -- 残りを5:5の比率で検証データとテストデータに分割
          (validData, testData) = splitAt (length restData * 5 `div` 10) restData
      smoothData' remainingData (trainDataAcc ++ trainData) (validDataAcc ++ validData) (testDataAcc ++ testData)

-- | メイン関数
-- コマンドライン引数からハイパーパラメータを取得し、
-- モデルの学習と評価を行います
main :: IO()
main = do
  -- コマンドライン引数の取得と解析
  args <- getArgs
  let bi = read (args !! 0) :: Bool        -- 双方向LSTMを使用するかどうか
      emb = read (args !! 1) :: Int        -- 埋め込み層の次元数
      h = read (args !! 2) :: Int          -- 隠れ層のサイズ
      l = read (args !! 3) :: Int          -- LSTMの層数
      bias = read (args !! 4) :: Bool      -- バイアスを使用するかどうか
      lr = read (args !! 5) :: Float       -- 学習率
      steps = read (args !! 6) :: Int      -- ステップ数
      iter = read (args !! 7) :: Int       -- エポック数
      delimiterToken = read (args !! 8) :: DelimiterToken  -- 区切り用トークンの種類

  -- waniTestデータセットの読み込み
  waniTestDataset <- loadActionsFromBinary proofSearchResultFilePath

  -- JSeMデータセットの読み込み
  jsemFiles <- listDirectory "data/JSeM/"
  jsemDatasets <- mapM (\file -> loadActionsFromBinary ("data/JSeM/" </> file)) jsemFiles

  -- データセットの前処理
  let dataset = waniTestDataset ++ concat jsemDatasets
      wordList = concatMap (getConstantSymbolsFromJudgment . fst) dataset
      frequentWords = getFrequentConstantSymbols wordList
      constructorData = map (\(judgment, _) -> splitJudgment judgment frequentWords delimiterToken) dataset
      ruleList = map snd dataset

  -- 規則の出現回数をカウントして表示
  let countedRules = countRule ruleList
  print $ "countedRules " ++ show countedRules

  -- データセットの分割
  splitedData <- splitByLabel (zip constructorData ruleList)
  (trainData, validData, testData) <- smoothData splitedData (Just 450)

  -- 訓練データの規則出現回数をカウントして表示
  let countedTrainRules = countRule $ map snd trainData
  print $ "countedRules (training data) " ++ show countedTrainRules

  -- ハイパーパラメータの設定
  let device = Device CPU 0                 -- 使用するデバイス（CPU/GPU）
      biDirectional = bi                    -- 双方向LSTMを使用するかどうか
      embDim = emb                          -- 埋め込み層の次元数
      numOfLayers = l                       -- LSTMの層数
      hiddenSize = h                        -- 隠れ層のサイズ
      hasBias = bias                        -- バイアスを使用するかどうか
      vocabSize = length allTokens          -- 語彙サイズ
      projSize = Nothing                    -- 投影サイズ（オプション）
      numOfRules = length allLabels         -- 規則の数
      hyperParams = HypParams device biDirectional embDim hasBias projSize vocabSize numOfLayers hiddenSize numOfRules
      learningRate = toDevice device (asTensor (lr :: Float))
      numberOfBatch = steps                 -- ステップ数

  -- ハイパーパラメータの表示
  print $ "hyperParams " ++ show hyperParams
  print $ "learningRate " ++ show learningRate
  print $ "numberOfBatch " ++ show numberOfBatch
  print $ "iter " ++ show iter
  print $ "delimiterToken " ++ show delimiterToken

  -- モデルの初期化
  initModel <- sample hyperParams
  let optimizer = mkAdam 0 0.9 0.999 (flattenParameters initModel)

  -- モデルの学習
  ((trainedModel), lossesPair) <- mapAccumM [1..iter] (initModel) $ \epoc (model) -> do
    -- 訓練データのシャッフル
    shuffledTrainData <- shuffleM trainData
    let batchedTrainData = List.chunksOf numberOfBatch shuffledTrainData
        batchedTrainData' = if length (last batchedTrainData) < numberOfBatch
                          then init batchedTrainData
                          else batchedTrainData

    -- バッチごとの学習
    ((trainedModel', _), lossPair) <- mapAccumM batchedTrainData' (model, 0 :: Int) $ \dataList (mdl, i) -> do
      performGC
      -- バッチ内の各データポイントに対する損失計算
      (sumLoss', losses) <- mapAccumM dataList (0 :: Tensor) $ \dat (accumulatedLoss) -> do
        output' <- forward device mdl dat biDirectional
        performGC
        let groundTruthIndex = toDevice device (asTensor [(fromEnum $ snd dat) :: Int])
            loss = nllLoss' groundTruthIndex output'
            lossValue = (asValue loss) :: Float
            sumLoss = accumulatedLoss + loss
        return (sumLoss, lossValue)

      -- モデルの更新
      (newModel, _) <- update mdl optimizer sumLoss' learningRate
      performGC

      -- 検証データに対する損失計算
      validLosses <- forM validData $ \dataPoint -> do
        validOutput' <- forward device mdl dataPoint biDirectional
        performGC
        let groundTruthIndex' = toDevice device (asTensor [(fromEnum $ snd dataPoint) :: Int])
            validLossValue = (asValue (nllLoss' groundTruthIndex' validOutput')) :: Float
        return validLossValue

      -- 損失の計算と表示
      let validLoss = sum validLosses / fromIntegral (length validLosses)
          trainLoss = sum losses / fromIntegral (length losses)
      print $ "epoch " ++ show epoc ++ " i " ++ show i ++ " trainingLoss " ++ show trainLoss ++ " validLoss " ++ show validLoss
      return ((newModel, i + 1), (trainLoss, validLoss))

    -- エポックごとの平均損失の計算と表示
    let (trainLoss', validLoss') = unzip lossPair
        avgTrainLoss = sum trainLoss' / fromIntegral (length trainLoss')
        avgValidLoss = sum validLoss' / fromIntegral (length validLoss')
    print $ "epoch " ++ show epoc ++ " avgTrainLoss " ++ show avgTrainLoss ++ " avgValidLoss " ++ show avgValidLoss
    print "----------------"

    return (trainedModel', (avgTrainLoss, avgValidLoss))

  -- 現在時刻の取得（ファイル名に使用）
  currentTime <- getZonedTime
  let timeString = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d_%H-%M-%S" (zonedTimeToLocalTime currentTime)
      modelFileName = "trained_data/seq-class" ++ timeString ++ ".model"
      graphFileName = "trained_data/graph-seq-class" ++ timeString ++ ".png"
      confusionMatrixFileName = "trained_data/confusion-matrix" ++ timeString ++ ".png"
      classificationReportFileName = "trained_data/classification-report" ++ timeString ++ ".txt"
      learningCurveTitle = "type: " ++ show delimiterToken ++ " bi: " ++ show biDirectional ++ " s: " ++ show numberOfBatch ++ " lr: " ++ show (asValue learningRate :: Float) ++  " i: " ++ show embDim ++ " h: " ++ show hiddenSize ++ " layer: " ++ show numOfLayers
      (losses, validLosses) = unzip lossesPair

  -- モデルの保存と学習曲線の描画
  saveParams trainedModel modelFileName
  drawLearningCurve graphFileName learningCurveTitle [("training", reverse losses), ("validation", reverse validLosses)]

  -- テストデータに対する予測と評価
  pairs <- forM testData $ \dataPoint -> do
    output' <- forward device trainedModel dataPoint biDirectional
    let groundTruthIndex = toDevice device (asTensor [(fromEnum $ snd dataPoint) :: Int])
        predictedClassIndex = argmax (Dim 1) KeepDim output'
        isCorrect = groundTruthIndex == predictedClassIndex
        label = toEnum (asValue predictedClassIndex :: Int) :: QT.DTTrule
    return (isCorrect, label)

  let (isCorrects, predictedLabel) = unzip pairs

  -- 予測結果の表示
  print $ zip predictedLabel (snd $ unzip $ testData)

  -- 分類レポートの生成と保存
  let classificationReport = showClassificationReport (length allLabels) (zip predictedLabel (snd $ unzip $ testData))
  T.putStr classificationReport

  B.writeFile classificationReportFileName (E.encodeUtf8 classificationReport)

  -- 混同行列の描画
  drawConfusionMatrix confusionMatrixFileName (length allLabels) (zip predictedLabel (snd $ unzip $ testData))

  -- 精度の計算と表示
  print $ "isCorrects " ++ show isCorrects

  let accuracy = (fromIntegral (length (filter id isCorrects)) / fromIntegral (length isCorrects)) :: Double
  print $ "Accuracy: " ++ show accuracy