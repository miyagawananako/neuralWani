import qualified Problems.SimpleProblems as SP (yes)
import qualified Problems.DifficultProblems as DP (yes)
import qualified Problems.NLPProblems as NLPP (yes)
import qualified ProblemBase as PB
import qualified DTS.QueryTypes as QT
import qualified DTS.DTTdeBruijn as U
import qualified Interface.Tree as I
import qualified Data.ByteString as B --bytestring
import Data.Store (encode)
import Data.Function (fix)
import ListT (toList)
import Control.Monad (forM)

saveFilePath :: FilePath
saveFilePath = "data/proofSearchResult"

makePair :: PB.TestType -> IO [(U.Judgment, QT.DTTrule)]
makePair ts = do
  let proofSearchResult = snd ts
  resultList <- toList proofSearchResult
  flip fix (resultList, []) $ \loop (searchResults, pairs) ->
    if null searchResults then return pairs
    else do
      let result = head searchResults
      loop (I.daughters result, pairs ++ [(I.node result, I.ruleName result)])

getProofSearchResult :: [PB.TestType] -> IO [(U.Judgment, QT.DTTrule)]
getProofSearchResult ts = do
  results <- forM ts makePair
  return $ concat results

main :: IO()
main = do
  searchResults <- getProofSearchResult (SP.yes ++ DP.yes ++ NLPP.yes)
  B.writeFile saveFilePath (encode searchResults)