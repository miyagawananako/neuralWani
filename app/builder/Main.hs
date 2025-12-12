import qualified DTS.Prover.Wani.BackwardRules as BR
import qualified DTS.Prover.Wani.WaniBase as WB
import qualified DTS.Prover.Wani.Arrowterm as A
import qualified Data.Text.Lazy as T

import NeuralWaniBuilder (neuralWaniBuilder)

goalExample :: WB.Goal
goalExample = WB.Goal
  [ (T.pack "entity", A.aType)
  , (T.pack "man", A.Arrow [A.aCon (T.pack "entity")] A.aType)
  ]                                             -- sig環境
  []                                            -- var環境：空
  Nothing                                       -- 推論モード
  [A.ArrowSigma' 
    [A.aCon (T.pack "entity")]                           -- x : entity
    (A.ArrowApp (A.aCon (T.pack "man")) (A.aVar 0))     -- man(x)
  ]

-- deduce'関数での規則の順番
--  [BR.PiForm]
--                           ++ (if arrowType /= (A.Conclusion DdB.Kind) then [BR.SigmaForm,BR.EqForm,BR.Membership,BR.AskOracle,BR.PiIntro,BR.SigmaIntro,BR.PiElim,BR.TopIntro,BR.DisjIntro,BR.DisjElim,BR.DisjForm] else [])
--                           ++ [BR.Dne | arrowType /= A.Conclusion DdB.Bot && WB.mode setting == WB.WithDNE && (arrowType /= (A.Conclusion DdB.Kind))]
--                           ++ [BR.Efq | arrowType /= A.Conclusion DdB.Bot && WB.mode setting == WB.WithEFQ && (arrowType /= (A.Conclusion DdB.Kind))]
--                         )

availableRules :: [BR.RuleLabel]
availableRules = [BR.Membership,BR.AskOracle,BR.PiIntro,BR.SigmaIntro,BR.PiElim,BR.TopIntro,BR.DisjIntro,BR.DisjElim,BR.Dne,BR.Efq]

main :: IO ()
main = do
  getPrioritizedRules <- neuralWaniBuilder
  let prioritizedRules = getPrioritizedRules goalExample availableRules
  print $ "prioritizedRules " ++ show prioritizedRules