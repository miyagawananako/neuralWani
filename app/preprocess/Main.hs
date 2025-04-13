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

proofSearchResultFilePath :: FilePath
proofSearchResultFilePath = "data/proofSearchResult"

getDataFromTestType :: PB.TestType -> IO [(U.Judgment, QT.DTTrule)]
getDataFromTestType testType = do
  let proofSearchResult = snd testType
  resultList <- toList proofSearchResult
  foldM processTree [] resultList
  where
    processTree pairs tree = do
      let daughters = I.daughters tree
          updatedPairs = (I.node tree, I.ruleName tree):pairs
      case daughters of
        [] -> return updatedPairs
        (d:ds) -> do
          pairs' <- processTree updatedPairs d
          foldM processTree pairs' ds

getProofSearchResults :: [PB.TestType] -> IO [(U.Judgment, QT.DTTrule)]
getProofSearchResults testType = do
  results <- forM testType getDataFromTestType
  return $ concat results

main :: IO()
main = do
  searchResults <- getProofSearchResults (SP.yes ++ DP.yes ++ NLPP.yes)
  print $ length searchResults
  B.writeFile proofSearchResultFilePath (encode searchResults)