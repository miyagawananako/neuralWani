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
  U.Con c  ->
    case () of
      _ | length frequentWords > 0 && c == frequentWords !! 0 -> [LPAREN] ++ [Word1] ++ [RPAREN]
        | length frequentWords > 1 && c == frequentWords !! 1 -> [LPAREN] ++ [Word2] ++ [RPAREN]
        | length frequentWords > 2 && c == frequentWords !! 2 -> [LPAREN] ++ [Word3] ++ [RPAREN]
        | length frequentWords > 3 && c == frequentWords !! 3 -> [LPAREN] ++ [Word4] ++ [RPAREN]
        | length frequentWords > 4 && c == frequentWords !! 4 -> [LPAREN] ++ [Word5] ++ [RPAREN]
        | length frequentWords > 5 && c == frequentWords !! 5 -> [LPAREN] ++ [Word6] ++ [RPAREN]
        | length frequentWords > 6 && c == frequentWords !! 6 -> [LPAREN] ++ [Word7] ++ [RPAREN]
        | length frequentWords > 7 && c == frequentWords !! 7 -> [LPAREN] ++ [Word8] ++ [RPAREN]
        | length frequentWords > 8 && c == frequentWords !! 8 -> [LPAREN] ++ [Word9] ++ [RPAREN]
        | length frequentWords > 9 && c == frequentWords !! 9 -> [LPAREN] ++ [Word10] ++ [RPAREN]
        | length frequentWords > 10 && c == frequentWords !! 10 -> [LPAREN] ++ [Word11] ++ [RPAREN]
        | length frequentWords > 11 && c == frequentWords !! 11 -> [LPAREN] ++ [Word12] ++ [RPAREN]
        | length frequentWords > 12 && c == frequentWords !! 12 -> [LPAREN] ++ [Word13] ++ [RPAREN]
        | length frequentWords > 13 && c == frequentWords !! 13 -> [LPAREN] ++ [Word14] ++ [RPAREN]
        | length frequentWords > 14 && c == frequentWords !! 14 -> [LPAREN] ++ [Word15] ++ [RPAREN]
        | length frequentWords > 15 && c == frequentWords !! 15 -> [LPAREN] ++ [Word16] ++ [RPAREN]
        | length frequentWords > 16 && c == frequentWords !! 16 -> [LPAREN] ++ [Word17] ++ [RPAREN]
        | length frequentWords > 17 && c == frequentWords !! 17 -> [LPAREN] ++ [Word18] ++ [RPAREN]
        | length frequentWords > 18 && c == frequentWords !! 18 -> [LPAREN] ++ [Word19] ++ [RPAREN]
        | length frequentWords > 19 && c == frequentWords !! 19 -> [LPAREN] ++ [Word20] ++ [RPAREN]
        | length frequentWords > 20 && c == frequentWords !! 20 -> [LPAREN] ++ [Word21] ++ [RPAREN]
        | length frequentWords > 21 && c == frequentWords !! 21 -> [LPAREN] ++ [Word22] ++ [RPAREN]
        | length frequentWords > 22 && c == frequentWords !! 22 -> [LPAREN] ++ [Word23] ++ [RPAREN]
        | length frequentWords > 23 && c == frequentWords !! 23 -> [LPAREN] ++ [Word24] ++ [RPAREN]
        | length frequentWords > 24 && c == frequentWords !! 24 -> [LPAREN] ++ [Word25] ++ [RPAREN]
        | length frequentWords > 25 && c == frequentWords !! 25 -> [LPAREN] ++ [Word26] ++ [RPAREN]
        | length frequentWords > 26 && c == frequentWords !! 26 -> [LPAREN] ++ [Word27] ++ [RPAREN]
        | length frequentWords > 27 && c == frequentWords !! 27 -> [LPAREN] ++ [Word28] ++ [RPAREN]
        | length frequentWords > 28 && c == frequentWords !! 28 -> [LPAREN] ++ [Word29] ++ [RPAREN]
        | length frequentWords > 29 && c == frequentWords !! 29 -> [LPAREN] ++ [Word30] ++ [RPAREN]
        | length frequentWords > 30 && c == frequentWords !! 30 -> [LPAREN] ++ [Word31] ++ [RPAREN]
        | otherwise -> [LPAREN] ++ [UNKNOWN] ++ [RPAREN]
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
splitSignature signature frequentWords = concatMap (\(name, preterm) ->
  case () of
    _ | length frequentWords > 0 && name == frequentWords !! 0 -> [LPAREN] ++ [Word1] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 1 && name == frequentWords !! 1 -> [LPAREN] ++ [Word2] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 2 && name == frequentWords !! 2 -> [LPAREN] ++ [Word3] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 3 && name == frequentWords !! 3 -> [LPAREN] ++ [Word4] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 4 && name == frequentWords !! 4 -> [LPAREN] ++ [Word5] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 5 && name == frequentWords !! 5 -> [LPAREN] ++ [Word6] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 6 && name == frequentWords !! 6 -> [LPAREN] ++ [Word7] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 7 && name == frequentWords !! 7 -> [LPAREN] ++ [Word8] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 8 && name == frequentWords !! 8 -> [LPAREN] ++ [Word9] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 9 && name == frequentWords !! 9 -> [LPAREN] ++ [Word10] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 10 && name == frequentWords !! 10 -> [LPAREN] ++ [Word11] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 11 && name == frequentWords !! 11 -> [LPAREN] ++ [Word12] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 12 && name == frequentWords !! 12 -> [LPAREN] ++ [Word13] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 13 && name == frequentWords !! 13 -> [LPAREN] ++ [Word14] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 14 && name == frequentWords !! 14 -> [LPAREN] ++ [Word15] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 15 && name == frequentWords !! 15 -> [LPAREN] ++ [Word16] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 16 && name == frequentWords !! 16 -> [LPAREN] ++ [Word17] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 17 && name == frequentWords !! 17 -> [LPAREN] ++ [Word18] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 18 && name == frequentWords !! 18 -> [LPAREN] ++ [Word19] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 19 && name == frequentWords !! 19 -> [LPAREN] ++ [Word20] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 20 && name == frequentWords !! 20 -> [LPAREN] ++ [Word21] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 21 && name == frequentWords !! 21 -> [LPAREN] ++ [Word22] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 22 && name == frequentWords !! 22 -> [LPAREN] ++ [Word23] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 23 && name == frequentWords !! 23 -> [LPAREN] ++ [Word24] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 24 && name == frequentWords !! 24 -> [LPAREN] ++ [Word25] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 25 && name == frequentWords !! 25 -> [LPAREN] ++ [Word26] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 26 && name == frequentWords !! 26 -> [LPAREN] ++ [Word27] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 27 && name == frequentWords !! 27 -> [LPAREN] ++ [Word28] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 28 && name == frequentWords !! 28 -> [LPAREN] ++ [Word29] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 29 && name == frequentWords !! 29 -> [LPAREN] ++ [Word30] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | length frequentWords > 30 && name == frequentWords !! 30 -> [LPAREN] ++ [Word31] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
      | otherwise -> [LPAREN] ++ [UNKNOWN] ++ [COMMA] ++ splitPreterm preterm frequentWords ++ [RPAREN]
  ) signature

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