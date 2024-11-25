import qualified DTS.QueryTypes as QT
import qualified DTS.DTTdeBruijn as U
import qualified Data.ByteString as B --bytestring
import qualified Data.Text.Lazy as T  --text
import Data.Store (decode)
import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Data.Ord (Down(..))

saveFilePath :: FilePath
saveFilePath = "data/proofSearchResult"

loadActionsFromBinary :: FilePath -> IO [(U.Judgment, QT.DTTrule)]
loadActionsFromBinary filepath = do
  binary <- B.readFile filepath
  case decode binary of
    Left peek_exception -> error $ "Could not parse dic file " ++ filepath ++ ": " ++ (show peek_exception)
    Right actions -> return actions

getWordsFromPreterm :: U.Preterm -> [T.Text]
getWordsFromPreterm preterm = case preterm of
  U.Con c  -> [c]
  U.Pi a b -> getWordsFromPreterm a ++ getWordsFromPreterm b
  U.Lam m  -> getWordsFromPreterm m
  U.App m n -> getWordsFromPreterm m ++ getWordsFromPreterm n
  U.Not m  -> getWordsFromPreterm m
  U.Sigma a b  -> getWordsFromPreterm a ++ getWordsFromPreterm b
  U.Pair m n   -> getWordsFromPreterm m ++ getWordsFromPreterm n
  U.Proj _ m   -> getWordsFromPreterm m
  U.Disj a b   -> getWordsFromPreterm a ++ getWordsFromPreterm b
  U.Iota _ m   -> getWordsFromPreterm m
  U.Unpack p h m n -> getWordsFromPreterm p ++ getWordsFromPreterm h ++ getWordsFromPreterm m ++ getWordsFromPreterm n
  U.Succ n     -> getWordsFromPreterm n
  U.Natrec n e f -> getWordsFromPreterm n ++ getWordsFromPreterm e ++ getWordsFromPreterm f
  U.Eq a m n   -> getWordsFromPreterm a ++ getWordsFromPreterm m ++ getWordsFromPreterm n
  U.Refl a m   -> getWordsFromPreterm a ++ getWordsFromPreterm m
  U.Idpeel m n -> getWordsFromPreterm m ++ getWordsFromPreterm n
  _ -> []

getWordsFromPreterms :: [U.Preterm] -> [T.Text]
getWordsFromPreterms preterms = concatMap (\preterm -> getWordsFromPreterm preterm) preterms

getWordsFromSignature :: U.Signature -> [T.Text]
getWordsFromSignature signature = concatMap (\(name, preterm) -> [name] ++ getWordsFromPreterm preterm) signature

getWordsFromJudgment :: U.Judgment -> [T.Text]
getWordsFromJudgment judgment =
  getWordsFromSignature (U.signtr judgment) ++
  getWordsFromPreterms (U.contxt judgment) ++
  getWordsFromPreterm (U.trm judgment) ++
  getWordsFromPreterm (U.typ judgment)

getFrequentWords :: [T.Text] -> [T.Text]
getFrequentWords frequentWords = take 31 $ map fst $ sortOn (Down . snd) $ Map.toList wordFreqMap
  where
    wordFreqMap :: Map.Map T.Text Int
    wordFreqMap = foldr (\word acc -> Map.insertWith (+) word 1 acc) Map.empty frequentWords

data IntermediateConstructor = EOSig | EOCon | EOTerm | EOTyp | FST | SND | LPAREN | RPAREN | COMMA
                              | Word1 | Word2 | Word3 | Word4 | Word5 | Word6 | Word7 | Word8 | Word9 | Word10 | Word11 | Word12 | Word13 | Word14 | Word15 | Word16 | Word17 | Word18 | Word19 | Word20 | Word21 | Word22 | Word23 | Word24 | Word25 | Word26 | Word27 | Word28 | Word29 | Word30 | Word31 | UNKNOWN
                              | Var'0 | Var'1 | Var'2 | Var'3 | Var'4 | Var'5 | Var'6 | Var'unknown
                              | Type' | Kind' | Pi' | Lam' | App' | Not' | Sigma' | Pair' | Proj' | Disj' | Iota' | Unpack' | Bot' | Unit' | Top' | Entity' | Nat' | Zero' | Succ' | Natrec' | Eq' | Refl' | Idpeel'
  deriving (Enum, Show)

textToIntermediateConstructor :: T.Text -> [T.Text] -> [IntermediateConstructor]
textToIntermediateConstructor text frequentWords =
  case () of
    _ | length frequentWords > 0 && text == frequentWords !! 0 -> [Word1]
      | length frequentWords > 1 && text == frequentWords !! 1 -> [Word2]
      | length frequentWords > 2 && text == frequentWords !! 2 -> [Word3]
      | length frequentWords > 3 && text == frequentWords !! 3 -> [Word4]
      | length frequentWords > 4 && text == frequentWords !! 4 -> [Word5]
      | length frequentWords > 5 && text == frequentWords !! 5 -> [Word6]
      | length frequentWords > 6 && text == frequentWords !! 6 -> [Word7]
      | length frequentWords > 7 && text == frequentWords !! 7 -> [Word8]
      | length frequentWords > 8 && text == frequentWords !! 8 -> [Word9]
      | length frequentWords > 9 && text == frequentWords !! 9 -> [Word10]
      | length frequentWords > 10 && text == frequentWords !! 10 -> [Word11]
      | length frequentWords > 11 && text == frequentWords !! 11 -> [Word12]
      | length frequentWords > 12 && text == frequentWords !! 12 -> [Word13]
      | length frequentWords > 13 && text == frequentWords !! 13 -> [Word14]
      | length frequentWords > 14 && text == frequentWords !! 14 -> [Word15]
      | length frequentWords > 15 && text == frequentWords !! 15 -> [Word16]
      | length frequentWords > 16 && text == frequentWords !! 16 -> [Word17]
      | length frequentWords > 17 && text == frequentWords !! 17 -> [Word18]
      | length frequentWords > 18 && text == frequentWords !! 18 -> [Word19]
      | length frequentWords > 19 && text == frequentWords !! 19 -> [Word20]
      | length frequentWords > 20 && text == frequentWords !! 20 -> [Word21]
      | length frequentWords > 21 && text == frequentWords !! 21 -> [Word22]
      | length frequentWords > 22 && text == frequentWords !! 22 -> [Word23]
      | length frequentWords > 23 && text == frequentWords !! 23 -> [Word24]
      | length frequentWords > 24 && text == frequentWords !! 24 -> [Word25]
      | length frequentWords > 25 && text == frequentWords !! 25 -> [Word26]
      | length frequentWords > 26 && text == frequentWords !! 26 -> [Word27]
      | length frequentWords > 27 && text == frequentWords !! 27 -> [Word28]
      | length frequentWords > 28 && text == frequentWords !! 28 -> [Word29]
      | length frequentWords > 29 && text == frequentWords !! 29 -> [Word30]
      | length frequentWords > 30 && text == frequentWords !! 30 -> [Word31]
      | otherwise -> [UNKNOWN]

splitPreterm :: U.Preterm -> [T.Text] -> [IntermediateConstructor]
splitPreterm preterm frequentWords = case preterm of
  U.Var j  ->
    case j of
      0 -> [LPAREN] ++ [Var'0] ++ [RPAREN]
      1 -> [LPAREN] ++ [Var'1] ++ [RPAREN]
      2 -> [LPAREN] ++ [Var'2] ++ [RPAREN]
      3 -> [LPAREN] ++ [Var'3] ++ [RPAREN]
      4 -> [LPAREN] ++ [Var'4] ++ [RPAREN]
      5 -> [LPAREN] ++ [Var'5] ++ [RPAREN]
      6 -> [LPAREN] ++ [Var'6] ++ [RPAREN]
      _ -> [LPAREN] ++ [Var'unknown] ++ [RPAREN]
  U.Con c  -> [LPAREN] ++ textToIntermediateConstructor c frequentWords ++ [RPAREN]
  U.Type   -> [LPAREN] ++ [Type'] ++ [RPAREN]
  U.Kind   -> [LPAREN] ++ [Kind'] ++ [RPAREN]
  U.Pi a b -> [LPAREN] ++ [Pi'] ++ splitPreterm a frequentWords ++ splitPreterm b frequentWords ++ [RPAREN]
  U.Lam m      -> [LPAREN] ++ [Lam'] ++ splitPreterm m frequentWords ++ [RPAREN]
  U.App m n    -> [LPAREN] ++ [App'] ++ splitPreterm m frequentWords ++ splitPreterm n frequentWords ++ [RPAREN]
  U.Not m  -> [LPAREN] ++ [Not'] ++ splitPreterm m frequentWords ++ [RPAREN]
  U.Sigma a b  -> [LPAREN] ++ [Sigma'] ++ splitPreterm a frequentWords ++ splitPreterm b frequentWords ++ [RPAREN]
  U.Pair m n   -> [LPAREN] ++ [Pair'] ++ splitPreterm m frequentWords ++ splitPreterm n frequentWords ++ [RPAREN]
  U.Proj s m   ->
    case s of
      U.Fst -> [LPAREN] ++ [Proj'] ++ [FST] ++ splitPreterm m frequentWords ++ [RPAREN]
      U.Snd -> [LPAREN] ++ [Proj'] ++ [SND] ++ splitPreterm m frequentWords ++ [RPAREN]
  U.Disj a b   -> [LPAREN] ++ [Disj'] ++ splitPreterm a frequentWords ++ splitPreterm b frequentWords ++ [RPAREN]
  U.Iota s m   ->
    case s of
      U.Fst -> [LPAREN] ++ [Iota'] ++ [FST] ++ splitPreterm m frequentWords ++ [RPAREN]
      U.Snd -> [LPAREN] ++ [Iota'] ++ [SND] ++ splitPreterm m frequentWords ++ [RPAREN]
  U.Unpack p h m n -> [LPAREN] ++ [Unpack'] ++ splitPreterm p frequentWords ++ splitPreterm h frequentWords ++ splitPreterm m frequentWords ++ splitPreterm n frequentWords ++ [RPAREN]
  U.Bot        -> [LPAREN] ++ [Bot'] ++ [RPAREN]
  U.Unit       -> [LPAREN] ++ [Unit'] ++ [RPAREN]
  U.Top        -> [LPAREN] ++ [Top'] ++ [RPAREN]
  U.Entity     -> [LPAREN] ++ [Entity'] ++ [RPAREN]
  U.Nat        -> [LPAREN] ++ [Nat'] ++ [RPAREN]
  U.Zero       -> [LPAREN] ++ [Zero'] ++ [RPAREN]
  U.Succ n     -> [LPAREN] ++ [Succ'] ++ splitPreterm n frequentWords ++ [RPAREN]
  U.Natrec n e f -> [LPAREN] ++ [Natrec'] ++ splitPreterm n frequentWords ++ splitPreterm e frequentWords ++ splitPreterm f frequentWords ++ [RPAREN]
  U.Eq a m n   -> [LPAREN] ++ [Eq'] ++ splitPreterm a frequentWords ++ splitPreterm m frequentWords ++ splitPreterm n frequentWords ++ [RPAREN]
  U.Refl a m   -> [LPAREN] ++ [Refl'] ++ splitPreterm a frequentWords ++ splitPreterm m frequentWords ++ [RPAREN]
  U.Idpeel m n -> [LPAREN] ++ [Idpeel'] ++ splitPreterm m frequentWords ++ splitPreterm n frequentWords ++ [RPAREN]

splitPreterms:: [U.Preterm] -> [T.Text]-> [IntermediateConstructor]
splitPreterms preterms frequentWords = concatMap (\preterm -> splitPreterm preterm frequentWords) preterms

splitSignature :: U.Signature -> [T.Text] -> [IntermediateConstructor]
splitSignature signature frequentWords = concatMap (\(name, preterm) -> [LPAREN] ++ textToIntermediateConstructor name frequentWords ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]) signature

splitJudgment :: U.Judgment -> [T.Text] -> [IntermediateConstructor]
splitJudgment judgment frequentWords =
  splitSignature (U.signtr judgment) frequentWords ++ [EOSig] ++
  splitPreterms (U.contxt judgment) frequentWords ++ [EOCon] ++
  splitPreterm (U.trm judgment) frequentWords ++ [EOTerm] ++
  splitPreterm (U.typ judgment) frequentWords ++ [EOTyp]

embed :: [IntermediateConstructor] -> [Int]
embed = map fromEnum

main :: IO()
main = do
  trainingData <- loadActionsFromBinary saveFilePath
  print trainingData

  let wordList = concatMap (\(judgment, _) -> getWordsFromJudgment judgment) trainingData
  -- print wordList
  let frequentWords = getFrequentWords wordList
  -- print frequentWords

  let constructorData = map (\(judgment, _) -> splitJudgment judgment frequentWords) trainingData
  -- print constructorData
  let embeddedData = map (\judgment -> embed judgment) constructorData
  print embeddedData