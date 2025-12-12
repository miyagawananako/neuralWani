import qualified DTS.Prover.Wani.WaniBase as WB
import qualified DTS.Prover.Wani.BackwardRules as BR
import qualified Data.ByteString as B --bytestring
import Data.Store (decode)
import Data.Maybe (catMaybes)
import Torch.Train (loadParams)
import qualified Forward as F
import Torch.Device (Device(..))

-- 本来lightblue内に置くパス（パスは例）
-- modelPath :: FilePath
-- modelPath = "trainedDataWithoutF/typeEo_biFalse_s32_lr5.0e-4_i256_h256_layer1/2025-12-10_20-46-49/seq-class.model"
-- frequentWordsPath :: FilePath
-- frequentWordsPath = "trainedDataWithoutF/typeEo_biFalse_s32_lr5.0e-4_i256_h256_layer1/2025-12-10_20-46-49/frequentWords.bin"

-- lightblue内に置く関数
-- getPrioritizedRules :: WB.Goal -> [WB.Rule] -> [WB.Rule] を作成して返す関数
-- dttRuleFromWaniBaseRuleがIOである以上、neuralWaniBuilderもIOである必要があるのが難しい点
-- neuralWaniBuilder :: IO (WB.Goal -> [WB.Rule] -> [WB.Rule])
-- neuralWaniBuilder = do
--   let device = Device CPU 0
--   model <- loadParams modelPath
--   frequentWords <- decode <$> B.readFile frequentWordsPath
--   let bi_directional = False
--   let delimiterToken = Unused
--   return $ \goal availableRules -> do
--     let maybeJudgment = WB.goal2NeuralWaniJudgement goal  -- Maybe DdB.Judgmentを受け取る
--     case maybeJudgment of
--       Just judgment -> do
--         let predictedRules = F.predictRule device model judgment bi_directional frequentWords delimiterToken  -- [QT.DTTrule]を受け取る
--         predictedWBRulesMaybe <- mapM (\rule -> BR.dttRuleFromWaniBaseRule rule) predictedRules  -- dttRuleFromWaniBaseRule :: QT.DTTrule -> IO (Maybe WB.Rule)
--         let predictedWBRules = catMaybes predictedWBRulesMaybe
--         return $ filter (\rule -> elem rule availableRules) predictedWBRules
--       Nothing -> do
--         return []


main :: IO ()
main = do
  -- getPrioritizedRules <- neuralWaniBuilder  -- deduce'関数での使用例
  print "neuralWaniBuilder is not implemented"