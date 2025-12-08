import qualified DTS.Prover.Wani.WaniBase as WB
import qualified DTS.Prover.Wani.BackwardRules as BR
import qualified Data.ByteString as B --bytestring
import Data.Store (decode)
import Data.Maybe (catMaybes)
import Torch.Serialize (loadParams)
import Torch.NN (sample)
import qualified Forward as F
import qualified DTS.QueryTypes as QT
import Torch.Device (Device(..),DeviceType(..))
import qualified SplitJudgment as S

-- 本来lightblue内に置くパス（パスは例）
modelPath :: FilePath
modelPath = "trainedDataWithoutF/typeUnused_biFalse_s32_lr5.0e-4_i256_h256_layer1/2025-11-20_02-08-09/seq-class.model"
frequentWordsPath :: FilePath
frequentWordsPath = "trainedDataWithoutF/typeUnused_biFalse_s32_lr5.0e-4_i256_h256_layer1/2025-11-20_02-08-09/frequentWords.bin"

-- lightblue内に置く関数
-- getPrioritizedRules :: WB.Goal -> [WB.Rule] -> [WB.Rule] を作成して返す関数
-- dttRuleFromWaniBaseRuleがIOである以上、neuralWaniBuilderもIOである必要があるのが難しい点
-- neuralWaniBuilder :: IO (WB.Goal -> [WB.Rule] -> [WB.Rule])
-- neuralWaniBuilder = do
--   let device = Device CPU 0
--       -- ハイパーパラメータの設定（trainedDataのパス名から推測: i256_h256_layer1）
--       hyperParams = F.HypParams
--         { F.dev = device
--         , F.bi_directional = False
--         , F.emb_dim = 256
--         , F.has_bias = True
--         , F.proj_size = Nothing
--         , F.vocab_size = length (enumFrom minBound :: [S.Token])
--         , F.num_layers = 1
  --       , F.hidden_size = 256
  --       , F.num_rules = length (enumFrom minBound :: [QT.DTTrule])
  --       }
  -- -- 空のモデルを初期化してからパラメータをロード
  -- emptyModel <- sample hyperParams
  -- model <- loadParams emptyModel modelPath
  -- frequentWordsEither <- decode <$> B.readFile frequentWordsPath
  -- frequentWords <- case frequentWordsEither of
  --   Left err -> error $ "Failed to decode frequentWords: " ++ show err
  --   Right words -> return words
  -- let bi_directional = False
  -- let delimiterToken = S.Unused
  -- return $ \goal availableRules -> do
  --   let maybeJudgment = WB.goal2NeuralWaniJudgement goal  -- Maybe DdB.Judgmentを受け取る
  --   case maybeJudgment of
  --     Just judgment -> do
  --       let predictedRules = F.predictRule device model judgment bi_directional frequentWords delimiterToken  -- [QT.DTTrule]を受け取る
  --       let predictedWBRulesMaybe = map (\rule -> BR.waniBaseRuleFromDTTrule rule) predictedRules  -- waniBaseRuleFromDTTrule :: QT.DTTrule -> Maybe WB.Rule
  --       let predictedWBRules = catMaybes predictedWBRulesMaybe
  --       return $ filter (\rule -> elem rule availableRules) predictedWBRules
  --     Nothing -> do
  --       return availableRules


main :: IO ()
main = do
  -- getPrioritizedRules <- neuralWaniBuilder  -- deduce'関数での使用例
  print "neuralWaniBuilder is not implemented"