import qualified Problems.SimpleProblems as SP (yes)
import qualified Problems.DifficultProblems as DP (yes)
import qualified Problems.NLPProblems as NLPP (yes)
import qualified ProblemBase as PB
import qualified DTS.QueryTypes as QT
import qualified DTS.DTTdeBruijn as U
import qualified Interface.Tree as I
import qualified Data.ByteString as B --bytestring
import Data.Store (encode)
import ListT (toList)
import Control.Monad (forM, foldM)

saveFilePath :: FilePath
saveFilePath = "data/proofSearchResult"

makePair :: PB.TestType -> IO [(U.Judgment, QT.DTTrule)]
makePair ts = do
  let proofSearchResult = snd ts
  resultList <- toList proofSearchResult
  foldM processTree [] resultList
  where
    processTree pairs tree = do
      let daughters = I.daughters tree
      let newPair = (I.node tree, I.ruleName tree)
      let updatedPairs = pairs ++ [newPair]
      if null daughters
        then return updatedPairs
        else foldM processTree updatedPairs daughters

getProofSearchResult :: [PB.TestType] -> IO [(U.Judgment, QT.DTTrule)]
getProofSearchResult ts = do
  results <- forM ts makePair
  return $ concat results

main :: IO()
main = do
  searchResults <- getProofSearchResult (SP.yes ++ DP.yes ++ NLPP.yes)
  B.writeFile saveFilePath (encode searchResults)