{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (forM)
import System.Random.Shuffle (shuffleM)
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)
import qualified Data.Text.IO as T    --text
import qualified Data.Text.Lazy as TL --text
import Data.Time.LocalTime
import qualified Data.Time as Time
import qualified Data.ByteString as B --bytestring
import Data.Store (encode)
import Data.Ord (Down(..))
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.List.Split as List
import           System.Environment (getArgs)
import System.Mem (performGC)
import System.Directory (createDirectoryIfMissing)
import qualified DTS.QueryTypes as QT
import qualified DTS.DTTdeBruijn as U
import qualified DTS.Prover.Wani.BackwardRules as BR
import Data.Maybe (mapMaybe)

--hasktorch関連のインポート
import Torch.Tensor       (Tensor(..),asValue, asTensor, toDevice)
import Torch.Device       (Device(..),DeviceType(..))
import Torch.Functional   (Dim(..),nllLoss')
import Torch.NN           (sample, flattenParameters)
import Torch.Optim        (mkAdam)
import Torch.Train        (update,saveParams)
import Torch.Control      (mapAccumM)

--可視化と評価用のツール
import ML.Exp.Chart   (drawLearningCurve) --nlp-tools

--プロジェクト固有のモジュール
import SplitJudgment (Token(..), loadActionsFromBinary, getConstantSymbolsFromJudgment, getFrequentConstantSymbols, splitJudgment, DelimiterToken(..), dttruleToRuleLabel, buildWordMap)
import Forward (HypParams(..), Params(..), forward)
import Evaluate (evaluateModel, saveEvaluationReport, EvaluationResult(..))

-- | データセットの種類を表すデータ型
data DatasetType = JSeM | TPTP String  -- TPTPはサブフォルダ名を含む
  deriving (Show, Read, Eq)

-- | すべてのラベル（RuleLabel）のリスト
allLabels :: [BR.RuleLabel]
allLabels = [minBound..]

-- | すべてのトークンのリスト
allTokens :: [Token]
allTokens = [minBound..]

-- | 元データのフィルタリング用のDTTruleリスト
backwardRules :: [QT.DTTrule]
backwardRules = [QT.PiF, QT.SigmaF, QT.IqF, QT.Var, QT.Con, QT.PiI, QT.SigmaI, QT.PiE, QT.TopI, QT.DisjI, QT.DisjE, QT.DisjF, QT.DNE, QT.EFQ]

formationRules :: [QT.DTTrule]
formationRules = [QT.TypeF, QT.PiF, QT.SigmaF, QT.DisjF, QT.BotF, QT.TopF, QT.EnumF, QT.IqF, QT.NatF]

-- | 規則の出現回数をカウントする関数
-- 各規則の出現頻度を計算し、頻度の降順でソートして返します
--
-- 引数：
-- * rules - 規則のリスト
--
-- 戻り値：
-- * (規則, 出現回数)のペアのリスト（出現回数の降順）
countRule :: [BR.RuleLabel] -> [(BR.RuleLabel, Int)]
countRule rules = List.sortOn (Down . snd) $ Map.toList ruleFreqMap
  where
    ruleFreqMap :: Map.Map BR.RuleLabel Int
    ruleFreqMap = foldr (\word acc -> Map.insertWith (+) word 1 acc) Map.empty rules

-- | データセットをラベル（規則）ごとに分割する関数
-- 各規則に対応するデータをグループ化します
--
-- 引数：
-- * dataset - (トークン列, 規則)のペアのリスト
--
-- 戻り値：
-- * (規則, その規則に対応するデータのリスト)のペアのリスト
splitByLabel :: [([Token], BR.RuleLabel)] -> IO [(BR.RuleLabel, [([Token], BR.RuleLabel)])]
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
smoothData :: [(BR.RuleLabel, [([Token], BR.RuleLabel)])] -> Maybe Int -> IO ([([Token], BR.RuleLabel)], [([Token], BR.RuleLabel)], [([Token], BR.RuleLabel)])
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

-- | モデルの学習を行う関数
-- 訓練データと検証データを使用してモデルを学習します
--
-- 引数：
-- * device - 使用するデバイス（CPU/GPU）
-- * hyperParams - ハイパーパラメータ
-- * trainData - 訓練データ
-- * validData - 検証データ
-- * biDirectional - 双方向LSTMを使用するかどうか
-- * iter - エポック数
-- * numberOfBatch - バッチサイズ
-- * learningRate - 学習率
-- * frequentWords - 頻出語のリスト（予測時にJudgmentをトークンに変換するために必要）
--
-- 戻り値：
-- * (学習済みモデル, 損失ペアのリスト, 頻出語のリスト)のタプル
-- * 損失ペアは(訓練損失, 検証損失)のリスト
trainModel :: Device -> HypParams -> [([Token], BR.RuleLabel)] -> [([Token], BR.RuleLabel)] -> Bool -> Int -> Int -> Tensor -> [TL.Text] -> IO (Params, [(Float, Float)], [TL.Text])
trainModel device hyperParams trainData validData biDirectional iter numberOfBatch learningRate frequentWords = do
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
        let output' = forward device mdl (fst dat) biDirectional
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
        let validOutput' = forward device mdl (fst dataPoint) biDirectional
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

  return (trainedModel, lossesPair, frequentWords)

-- | データセットの種類をパースする
-- "jsem" -> JSeM
-- "tptp:フォルダ名" -> TPTP フォルダ名
parseDatasetType :: String -> DatasetType
parseDatasetType "jsem" = JSeM
parseDatasetType s
  | "tptp:" `List.isPrefixOf` s = TPTP (drop 5 s)
  | otherwise = error $ "Unknown dataset type: " ++ s ++ ". Use 'jsem' or 'tptp:folder_name'"

-- | データセットを読み込む関数
-- JSeM: data/JSeM/ 内のすべてのファイルを読み込む
-- TPTP: extractedData/指定フォルダ/ 内のすべての.binファイルを読み込む
loadDataset :: DatasetType -> IO [(U.Judgment, QT.DTTrule)]
loadDataset JSeM = do
  jsemFiles <- listDirectory "data/JSeM/"
  datasets <- mapM (\file -> loadActionsFromBinary ("data/JSeM/" </> file)) jsemFiles
  return $ concat datasets
loadDataset (TPTP folderName) = do
  let tptpPath = "extractedData" </> folderName
  exists <- doesDirectoryExist tptpPath
  if not exists
    then error $ "TPTP folder not found: " ++ tptpPath
    else do
      allFiles <- listDirectory tptpPath
      let binFiles = filter (\f -> takeExtension f == ".bin") allFiles
      if null binFiles
        then error $ "No .bin files found in: " ++ tptpPath
        else do
          print $ "Loading " ++ show (length binFiles) ++ " .bin files from " ++ tptpPath
          datasets <- mapM (\file -> loadActionsFromBinary (tptpPath </> file)) binFiles
          return $ concat datasets

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
      datasetTypeStr = args !! 9           -- データセットの種類 ("jsem" or "tptp:folder_name")
      datasetType = parseDatasetType datasetTypeStr

  -- データセットの読み込み
  print $ "Loading dataset: " ++ show datasetType
  originalDataset <- loadDataset datasetType

  -- 形成則を含めるかどうか
  let isIncludeF = False
      isOnlyBackwardRules = True

  -- データセットの前処理
  -- 元データはQT.DTTruleのまま読み込み、フィルタリング後にBR.RuleLabelに変換
  let backwardDataset = if isOnlyBackwardRules
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
      -- 頻出語リストをMapに事前変換（高速化のため）
      wordMap = buildWordMap frequentWords
      constructorData = map (\(judgment, _) -> splitJudgment judgment wordMap delimiterToken) dataset
      ruleList = map snd dataset

  -- 規則の出現回数をカウントして表示
  let countedRules = countRule ruleList
  print $ "countedRules " ++ show countedRules

  -- データセットの分割
  splitedData <- splitByLabel (zip constructorData ruleList)
  (trainData, validData, testData) <- smoothData splitedData (Nothing)

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
      numOfRules = length (enumFrom minBound :: [BR.RuleLabel])  -- RuleLabelの数を使用
      hyperParams = HypParams device biDirectional embDim hasBias projSize vocabSize numOfLayers hiddenSize numOfRules
      learningRate = toDevice device (asTensor (lr :: Float))
      numberOfBatch = steps                 -- ステップ数

  -- ハイパーパラメータの表示
  print $ "hyperParams " ++ show hyperParams
  print $ "learningRate " ++ show learningRate
  print $ "numberOfBatch " ++ show numberOfBatch
  print $ "iter " ++ show iter
  print $ "delimiterToken " ++ show delimiterToken

  -- 学習開始時刻の記録
  startTime <- Time.getCurrentTime
  print $ "Training started at: " ++ show startTime

  -- モデルの学習
  (trainedModel, lossesPair, frequentWords') <- trainModel device hyperParams trainData validData biDirectional iter numberOfBatch learningRate frequentWords

  -- 学習終了時刻の記録と学習時間の計算
  endTime <- Time.getCurrentTime
  let trainingDuration = Time.diffUTCTime endTime startTime
  print $ "Training finished at: " ++ show endTime
  print $ "Total training time: " ++ show trainingDuration

  -- 現在時刻の取得（フォルダ名に使用）
  currentTime <- getZonedTime
  let timeString = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d_%H-%M-%S" (zonedTimeToLocalTime currentTime)
      datasetSuffix = case datasetType of
                        JSeM -> "jsem"
                        TPTP folder -> "tptp_" ++ folder
      baseFolderName = case (isIncludeF, isOnlyBackwardRules) of
                        (True, True)   -> "trainedDataBackward"
                        (True, False)  -> "trainedData"
                        (False, True)  -> "trainedDataBackwardWithoutF"
                        (False, False) -> "trainedDataWithoutF"
      newFolderPath = baseFolderName ++ "/" ++ datasetSuffix ++ "_type" ++ show delimiterToken ++ "_bi" ++ show biDirectional ++ "_s" ++ show numberOfBatch ++ "_lr" ++ show (asValue learningRate :: Float) ++  "_i" ++ show embDim ++ "_h" ++ show hiddenSize ++ "_layer" ++ show numOfLayers ++ "/" ++ timeString

  createDirectoryIfMissing True newFolderPath

  let modelFileName = newFolderPath ++ "/seq-class" ++ ".model"
      frequentWordsFileName = newFolderPath ++ "/frequentWords" ++ ".bin"
      graphFileName =  newFolderPath ++ "/graph-seq-class"  ++ ".png"
      trainingTimeFileName = newFolderPath ++ "/training-time" ++ ".txt"
      learningCurveTitle = "type: " ++ show delimiterToken ++ " bi: " ++ show biDirectional ++ " s: " ++ show numberOfBatch ++ " lr: " ++ show (asValue learningRate :: Float) ++  " i: " ++ show embDim ++ " h: " ++ show hiddenSize ++ " layer: " ++ show numOfLayers
      (losses, validLosses) = unzip lossesPair

  -- モデルとfrequentWordsの保存と学習曲線の描画
  saveParams trainedModel modelFileName
  B.writeFile frequentWordsFileName (encode frequentWords')

  drawLearningCurve graphFileName learningCurveTitle [("training", reverse losses), ("validation", reverse validLosses)]

  -- テストデータに対する予測と評価（Evaluateモジュールを使用）
  evalResult <- evaluateModel device trainedModel testData biDirectional
  
  -- 予測結果の表示
  print $ erPredictions evalResult
  
  -- 分類レポートの表示
  T.putStr $ TL.toStrict $ erClassificationReport evalResult
  
  -- 学習時間の保存
  let trainingTimeReport = TL.pack $ "Training Duration: " ++ show trainingDuration ++ "\n" ++
                                      "Start Time: " ++ show startTime ++ "\n" ++
                                      "End Time: " ++ show endTime ++ "\n"
  T.writeFile trainingTimeFileName (TL.toStrict trainingTimeReport)
  
  -- 評価結果をファイルに保存（分類レポートと混同行列）
  saveEvaluationReport newFolderPath evalResult allLabels
  
  -- 精度の表示
  print $ "isCorrects " ++ show (erCorrectFlags evalResult)
  print $ "Accuracy: " ++ show (erAccuracy evalResult)