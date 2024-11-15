import qualified Problems.SimpleProblems as SP (yes)
import qualified Problems.DifficultProblems as DP (yes)
import qualified Problems.NLPProblems as NLPP (yes)
import qualified DTS.QueryTypes as QT
import qualified DTS.DTTdeBruijn as U
import qualified Interface.Tree as I
import qualified Data.ByteString as B --bytestring
import qualified Data.Text.Lazy as T  --text
import Data.Store (encode, decode)
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

loadActionsFromBinary :: FilePath -> IO [(U.Judgment, QT.DTTrule)]
loadActionsFromBinary filepath = do
  binary <- B.readFile filepath
  case decode binary of
    Left peek_exception -> error $ "Could not parse dic file " ++ filepath ++ ": " ++ (show peek_exception)
    Right actions -> return actions

hash :: T.Text -> Int
hash = T.foldl' (\acc c -> acc * 31 + fromEnum c) 0

embedPreterm :: U.Preterm -> [Int]
embedPreterm preterm = case preterm of
  U.Var j  -> [1, j]
  U.Con c  -> [2, hash c]
  U.Type   -> [3]
  U.Kind   -> [4]
  U.Pi a b -> [5] ++ embedPreterm a ++ embedPreterm b
  U.Lam m      -> [6] ++ embedPreterm m
  U.App m n    -> [7] ++ embedPreterm m ++ embedPreterm n
  U.Not m  -> [8] ++ embedPreterm m
  U.Sigma a b  -> [9] ++ embedPreterm a ++ embedPreterm b
  U.Pair m n   -> [10] ++ embedPreterm m ++ embedPreterm n
  U.Proj s m   ->
    case s of
      U.Fst -> [11, 1] ++ embedPreterm m
      U.Snd -> [11, 2] ++ embedPreterm m
  U.Disj a b   -> [12] ++ embedPreterm a ++ embedPreterm b
  U.Iota s m   ->
    case s of
      U.Fst -> [13, 1] ++ embedPreterm m
      U.Snd -> [13, 2] ++ embedPreterm m
  U.Unpack p h m n -> [14] ++ embedPreterm p ++ embedPreterm h ++ embedPreterm m ++ embedPreterm n
  U.Bot        -> [15]
  U.Unit       -> [16]
  U.Top        -> [17]
  U.Entity     -> [18]
  U.Nat        -> [19]
  U.Zero       -> [20]
  U.Succ n     -> [21] ++ embedPreterm n
  U.Natrec n e f -> [22] ++ embedPreterm n ++ embedPreterm e ++ embedPreterm f
  U.Eq a m n   -> [23] ++ embedPreterm a ++ embedPreterm m ++ embedPreterm n
  U.Refl a m   -> [24] ++ embedPreterm a ++ embedPreterm m
  U.Idpeel m n -> [25] ++ embedPreterm m ++ embedPreterm n

embedPreterms :: [U.Preterm] -> [Int]
embedPreterms preterms = concatMap (\preterm -> embedPreterm preterm ++ [-1]) preterms

embedSignature :: U.Signature -> [Int]
embedSignature signature = concatMap (\(name, preterm) -> [hash name] ++ embedPreterm preterm ++ [-1]) signature

main :: IO()
main = do
  searchResults <- getProofSearchResult (SP.yes ++ DP.yes ++ NLPP.yes)
  B.writeFile saveFilePath (encode searchResults)

  trainingData <- loadActionsFromBinary saveFilePath

  let judgmentData = map (\(judgment, _) -> (embedSignature $ U.signtr judgment) ++ [-2] ++ (embedPreterms $ U.contxt judgment) ++ [-3] ++ (embedPreterm $ U.trm judgment) ++ [-4] ++ (embedPreterm $ U.typ judgment)) trainingData
  print judgmentData