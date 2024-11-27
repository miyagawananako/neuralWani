import qualified DTS.QueryTypes as QT
import qualified DTS.DTTdeBruijn as U
import qualified Data.ByteString as B --bytestring
import qualified Data.Text.Lazy as T  --text
import Data.Store (decode)
import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Data.Ord (Down(..))
import qualified Data.Set as Set

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

allowDuplicateWords :: Bool
allowDuplicateWords = True

getWordsFromJudgment :: U.Judgment -> [T.Text]
getWordsFromJudgment judgment =
  if allowDuplicateWords then wordList
  else Set.toList . Set.fromList $ wordList
  where
    wordList =
      getWordsFromSignature (U.signtr judgment) ++
      getWordsFromPreterms (U.contxt judgment) ++
      getWordsFromPreterm (U.trm judgment) ++
      getWordsFromPreterm (U.typ judgment)

getFrequentWords :: [T.Text] -> [T.Text]
getFrequentWords frequentWords = take 31 $ map fst $ sortOn (Down . snd) $ Map.toList wordFreqMap
  where
    wordFreqMap :: Map.Map T.Text Int
    wordFreqMap = foldr (\word acc -> Map.insertWith (+) word 1 acc) Map.empty frequentWords

data Token = EOSig | EOCon | EOTerm | EOTyp | FST | SND | LPAREN | RPAREN | COMMA
                              | Word1 | Word2 | Word3 | Word4 | Word5 | Word6 | Word7 | Word8 | Word9 | Word10 | Word11 | Word12 | Word13 | Word14 | Word15 | Word16 | Word17 | Word18 | Word19 | Word20 | Word21 | Word22 | Word23 | Word24 | Word25 | Word26 | Word27 | Word28 | Word29 | Word30 | Word31 | UNKNOWN
                              | Var'0 | Var'1 | Var'2 | Var'3 | Var'4 | Var'5 | Var'6 | Var'unknown
                              | Type' | Kind' | Pi' | Lam' | App' | Not' | Sigma' | Pair' | Proj' | Disj' | Iota' | Unpack' | Bot' | Unit' | Top' | Entity' | Nat' | Zero' | Succ' | Natrec' | Eq' | Refl' | Idpeel'
  deriving (Enum, Show)

isParen :: Bool
isParen = True

wrapWithParen :: [Token] -> [Token]
wrapWithParen xs = if isParen then [LPAREN] ++ xs ++ [RPAREN] else xs

textToToken :: T.Text -> [T.Text] -> [Token]
textToToken text frequentWords =
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

varToToken :: Int -> [Token]
varToToken i =
  case i of
    0 -> [Var'0]
    1 -> [Var'1]
    2 -> [Var'2]
    3 -> [Var'3]
    4 -> [Var'4]
    5 -> [Var'5]
    6 -> [Var'6]
    _ -> [Var'unknown]

selectorToToken :: U.Selector -> [Token]
selectorToToken s = case s of
  U.Fst -> [FST]
  U.Snd -> [SND]

splitPreterm :: U.Preterm -> [T.Text] -> [Token]
splitPreterm preterm frequentWords = case preterm of
  U.Var i -> wrapWithParen (varToToken i)
  U.Con c -> wrapWithParen (textToToken c frequentWords)
  U.Type -> wrapWithParen [Type']
  U.Kind -> wrapWithParen [Kind']
  U.Pi a b -> wrapWithParen ([Pi'] ++ splitPreterm a frequentWords ++ splitPreterm b frequentWords)
  U.Lam m -> wrapWithParen ([Lam'] ++ splitPreterm m frequentWords)
  U.App m n -> wrapWithParen ([App'] ++ splitPreterm m frequentWords ++ splitPreterm n frequentWords)
  U.Not m -> wrapWithParen ([Not'] ++ splitPreterm m frequentWords)
  U.Sigma a b -> wrapWithParen ([Sigma'] ++ splitPreterm a frequentWords ++ splitPreterm b frequentWords)
  U.Pair m n -> wrapWithParen ([Pair'] ++ splitPreterm m frequentWords ++ splitPreterm n frequentWords)
  U.Proj s m -> wrapWithParen ([Proj'] ++ selectorToToken s ++ splitPreterm m frequentWords)
  U.Disj a b -> wrapWithParen ([Disj'] ++ splitPreterm a frequentWords ++ splitPreterm b frequentWords)
  U.Iota s m -> wrapWithParen ([Iota'] ++ selectorToToken s ++ splitPreterm m frequentWords)
  U.Unpack p h m n -> wrapWithParen ([Unpack'] ++ splitPreterm p frequentWords ++ splitPreterm h frequentWords ++ splitPreterm m frequentWords ++ splitPreterm n frequentWords)
  U.Bot -> wrapWithParen [Bot']
  U.Unit -> wrapWithParen [Unit']
  U.Top -> wrapWithParen [Top']
  U.Entity -> wrapWithParen [Entity']
  U.Nat -> wrapWithParen [Nat']
  U.Zero -> wrapWithParen [Zero']
  U.Succ m -> wrapWithParen ([Succ'] ++ splitPreterm m frequentWords)
  U.Natrec n e f -> wrapWithParen ([Natrec'] ++ splitPreterm n frequentWords ++ splitPreterm e frequentWords ++ splitPreterm f frequentWords)
  U.Eq a m n -> wrapWithParen ([Eq'] ++ splitPreterm a frequentWords ++ splitPreterm m frequentWords ++ splitPreterm n frequentWords)
  U.Refl a m -> wrapWithParen ([Refl'] ++ splitPreterm a frequentWords ++ splitPreterm m frequentWords)
  U.Idpeel m n -> wrapWithParen ([Idpeel'] ++ splitPreterm m frequentWords ++ splitPreterm n frequentWords)

splitPreterms:: [U.Preterm] -> [T.Text]-> [Token]
splitPreterms preterms frequentWords = concatMap (\preterm -> splitPreterm preterm frequentWords) preterms

splitSignature :: U.Signature -> [T.Text] -> [Token]
splitSignature signature frequentWords = concatMap (\(name, preterm) -> wrapWithParen (textToToken name frequentWords ++ [COMMA] ++ splitPreterm preterm frequentWords)) signature

splitJudgment :: U.Judgment -> [T.Text] -> [Token]
splitJudgment judgment frequentWords =
  splitSignature (U.signtr judgment) frequentWords ++ [EOSig] ++
  splitPreterms (U.contxt judgment) frequentWords ++ [EOCon] ++
  splitPreterm (U.trm judgment) frequentWords ++ [EOTerm] ++
  splitPreterm (U.typ judgment) frequentWords ++ [EOTyp]

embed :: [Token] -> [Int]
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