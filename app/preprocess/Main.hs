import qualified Problems.SimpleProblems as SP (yes)
import qualified Problems.DifficultProblems as DP (yes)
import qualified Problems.NLPProblems as NLPP (yes)
import qualified DTS.QueryTypes as QT
import qualified DTS.DTTdeBruijn as U
import qualified Interface.Tree as I
import qualified Data.ByteString as B --bytestring
import Data.Store (encode)
import ListT (ListT, toList)
import Control.Monad (forM)

type ProofSearchResult = ListT IO (I.Tree QT.DTTrule (U.Judgment))
type TestType = (Bool, ProofSearchResult) -- ^ (predicted, result)

saveFilePath :: FilePath
saveFilePath = "data/proofSearchResult"

getProofSearchResult :: [TestType] -> IO [(U.Judgment, QT.DTTrule)]
getProofSearchResult ts = do
  results <- forM ts $ \(_, result) -> do
    resultList <- toList result
    return resultList
  return (map (\tree -> (I.node tree, I.ruleName tree)) (concat results))

main :: IO()
main = do
  searchResults <- getProofSearchResult (SP.yes ++ DP.yes ++ NLPP.yes)
  B.writeFile saveFilePath (encode searchResults)