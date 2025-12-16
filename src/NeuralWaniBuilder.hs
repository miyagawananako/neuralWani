module NeuralWaniBuilder
  ( neuralWaniBuilder
  , modelPath
  , frequentWordsPath
  ) where

import qualified DTS.Prover.Wani.WaniBase as WB
import qualified DTS.Prover.Wani.BackwardRules as BR

import qualified Data.ByteString as B --bytestring
import Data.Store (decode)

import Torch.Serialize (loadParams)
import Torch.NN (sample)
import Torch.Device (Device(..),DeviceType(..))

import qualified Forward as F
import qualified SplitJudgment as S

-- 本来lightblue内に置くパス（パスは仮）。CUDA でしか開けない
-- modelPath :: FilePath
-- modelPath = "trainedDataWithoutF/typeEo_biFalse_s32_lr5.0e-4_i256_h256_layer1/2025-12-10_20-46-49/seq-class.model"
-- frequentWordsPath :: FilePath
-- frequentWordsPath = "trainedDataWithoutF/typeEo_biFalse_s32_lr5.0e-4_i256_h256_layer1/2025-12-10_20-46-49/frequentWords.bin"

-- CPU用のパス
modelPath :: FilePath
modelPath = "trainedDataBackwardWithoutF/typeUnused_biFalse_s32_lr5.0e-4_i256_h256_layer1/2025-12-12_06-53-21/seq-class.model"
frequentWordsPath :: FilePath
frequentWordsPath = "trainedDataBackwardWithoutF/typeUnused_biFalse_s32_lr5.0e-4_i256_h256_layer1/2025-12-12_06-53-21/frequentWords.bin"


-- lightblue内に置く関数
neuralWaniBuilder :: IO (WB.Goal -> [BR.RuleLabel] -> [BR.RuleLabel])
neuralWaniBuilder = do
  let device = Device CPU 0
      hyperParams = F.HypParams
        { F.dev = device
        , F.bi_directional = False
        , F.emb_dim = 256
        , F.has_bias = False  -- 訓練時と同じ値に設定
        , F.proj_size = Nothing
        , F.vocab_size = length (enumFrom minBound :: [S.Token])
        , F.num_layers = 1
        , F.hidden_size = 256
        , F.num_rules = length (enumFrom minBound :: [BR.RuleLabel])
        }
      bi_directional = False
      delimiterToken = S.Unused
  emptyModel <- sample hyperParams
  model <- loadParams emptyModel modelPath
  frequentWordsEither <- decode <$> B.readFile frequentWordsPath
  frequentWords <- case frequentWordsEither of
    Left err -> error $ "Failed to decode frequentWords: " ++ show err
    Right ws -> return ws
  -- 頻出語リストをMapに事前変換（高速化のため）
  let wordMap = S.buildWordMap frequentWords
  return $ \goal availableRuleLabels ->
    let maybeJudgment = WB.goal2NeuralWaniJudgement goal
    in case maybeJudgment of
      Just judgment ->
        let predictedRuleLabels = F.predictRule device model judgment bi_directional wordMap delimiterToken
            filteredRuleLabels = filter (`elem` availableRuleLabels) predictedRuleLabels
        in filteredRuleLabels
      Nothing -> availableRuleLabels

