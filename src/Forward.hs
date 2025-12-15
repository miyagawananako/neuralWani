{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Forward where

import GHC.Generics                   --base
import Data.Ord (Down(..))
import qualified Data.List as List
import qualified DTS.QueryTypes as QT
import qualified DTS.DTTdeBruijn as U
import qualified DTS.Prover.Wani.BackwardRules as BR

--hasktorch関連のインポート
import Torch.Tensor       (Tensor(..),asValue,reshape, shape, asTensor, sliceDim, toDevice)
import Torch.Device       (Device(..),DeviceType(..))
import Torch.Functional   (Dim(..), embedding', logSoftmax)
import Torch.NN           (Parameter,Parameterized,Randomizable,sample)
import Torch.Autograd     (makeIndependent,toDependent)
import Torch.Tensor.TensorFactories (randnIO')
import Torch.Layer.Linear (LinearHypParams(..),LinearParams,linearLayer)
import Torch.Layer.LSTM   (LstmHypParams(..),LstmParams,lstmLayers)

--プロジェクト固有のモジュール
import SplitJudgment (Token(..), splitJudgment, DelimiterToken(..), WordMap)

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
-- * tokens - 入力データ（トークン列）
-- * bi_directional - 双方向LSTMを使用するかどうか
--
-- 戻り値：
-- * 各規則の予測確率（対数ソフトマックス適用済み）
forward :: Device -> Params -> [Token] -> Bool -> Tensor
forward device Params{..} tokens bi_directional =
  -- トークンをインデックスに変換し、テンソルに変換
  let inputIndices = map fromEnum tokens
      idxs = toDevice device $ asTensor (inputIndices :: [Int])
      -- 埋め込み層を適用
      input = embedding' (toDependent w_emb) idxs
      dropout_prob = Nothing
      -- LSTM層を適用
      (_, (h, _)) = lstmLayers lstm_params dropout_prob hc $ input
      -- 最後の層の出力を取得（双方向の場合は両方向の出力を結合）
      lastOutput = extractLastOutput h bi_directional
      -- 全結合層とソフトマックスを適用
      output = linearLayer mlp_params lastOutput
      output' = logSoftmax (Dim 1) output
  in output'

-- | LSTMの最後の層の出力を抽出する関数
-- 双方向LSTMの場合は両方向の出力を結合します
--
-- 引数：
-- * tensor - LSTMの出力テンソル
-- * bi_directional - 双方向LSTMを使用するかどうか
--
-- 戻り値：
-- * 最後の層の出力（双方向の場合は結合された出力）
extractLastOutput :: Tensor -> Bool -> Tensor
extractLastOutput tensor bi_directional =
  let shapeInput = shape tensor
  in case bi_directional of
    True ->
      -- 双方向の場合、最後の2つの出力を取得して結合
      let lastOutput1 = sliceDim 0 (shapeInput !! 0 - 2) (shapeInput !! 0) 1 tensor  -- [2, hidden_size]
      in reshape [1, 2 * (shapeInput !! (length shapeInput - 1))] lastOutput1  -- [1, 2 * hidden_size]
    False ->
      -- 単方向の場合、最後の出力を取得
      case shapeInput of
        [1, _] -> tensor
        [_, _] -> sliceDim 0 (shapeInput !! 0 - 1) (shapeInput !! 0) 1 tensor
        _      -> error $ "Unexpected shape: " ++ show shapeInput

-- | Judgmentから規則を予測する関数
-- forward関数とsplitJudgment関数を活用して、Judgmentから規則を予測します
--
-- 引数：
-- * device - 使用するデバイス（CPU/GPU）
-- * params - モデルのパラメータ
-- * judgment - 予測対象のJudgment
-- * bi_directional - 双方向LSTMを使用するかどうか
-- * wordMap - 頻出語からトークンへのマッピング（buildWordMapで事前構築）
-- * delimiterToken - 区切り用トークンの種類（splitJudgmentに必要）
--
-- 戻り値：
-- * 予測された規則のリスト（確率の高い順にソート済み、BR.RuleLabel）
predictRule :: Device -> Params -> U.Judgment -> Bool -> WordMap -> DelimiterToken -> [BR.RuleLabel]
predictRule device params judgment bi_directional wordMap delimiterToken =
  -- Judgmentをトークン列に変換
  let tokens = splitJudgment judgment wordMap delimiterToken
      -- forward関数を呼び出して予測確率テンソルを取得
      output' = forward device params tokens bi_directional
      -- テンソルから値を取得（形状は[1, num_rules]）
      probabilities :: [[Float]]
      probabilities = asValue output'
      -- 最初のバッチ（バッチサイズ1なので最初の要素）を取得
      probs = head probabilities
      -- インデックスと確率をペアにして、確率の降順でソート
      indexedProbs = zip [0..] probs
      sortedIndexedProbs = List.sortOn (Down . snd) indexedProbs
      -- インデックスを規則に変換（BR.RuleLabelとして）
      predictedRules = map (\(idx, _) -> toEnum idx :: BR.RuleLabel) sortedIndexedProbs
  in predictedRules

