import qualified DTS.QueryTypes as QT
import qualified DTS.DTTdeBruijn as U
import qualified Data.ByteString as B --bytestring
import qualified Data.Text.Lazy as T  --text
import Data.Store (decode)

saveFilePath :: FilePath
saveFilePath = "data/proofSearchResult"

loadActionsFromBinary :: FilePath -> IO [(U.Judgment, QT.DTTrule)]
loadActionsFromBinary filepath = do
  binary <- B.readFile filepath
  case decode binary of
    Left peek_exception -> error $ "Could not parse dic file " ++ filepath ++ ": " ++ (show peek_exception)
    Right actions -> return actions

hash :: T.Text -> Int
hash = T.foldl' (\acc c -> acc * 31 + fromEnum c) 0

data IntermediateConstructor = EOSig | EOCon | EOTerm | EOTyp | FST | SND | LPAREN | RPAREN | OTHER | Var'0 | Var'1 | Var'2 | Var'3 | Var'4 | Var'5 | Var'6 | Con' | Type' | Kind' | Pi' | Lam' | App' | Not' | Sigma' | Pair' | Proj' | Disj' | Iota' | Unpack' | Bot' | Unit' | Top' | Entity' | Nat' | Zero' | Succ' | Natrec' | Eq' | Refl' | Idpeel'
  deriving (Enum, Show)

splitPreterm :: U.Preterm -> [IntermediateConstructor]
splitPreterm preterm = case preterm of
  U.Var j  ->
    case j of
      0 -> [LPAREN] ++ [Var'0] ++ [RPAREN]
      1 -> [LPAREN] ++ [Var'1] ++ [RPAREN]
      2 -> [LPAREN] ++ [Var'2] ++ [RPAREN]
      3 -> [LPAREN] ++ [Var'3] ++ [RPAREN]
      4 -> [LPAREN] ++ [Var'4] ++ [RPAREN]
      5 -> [LPAREN] ++ [Var'5] ++ [RPAREN]
      6 -> [LPAREN] ++ [Var'6] ++ [RPAREN]
      _ -> [LPAREN] ++ [OTHER] ++ [RPAREN]
  U.Con c  -> [LPAREN] ++ [Con'] ++ [RPAREN]
  U.Type   -> [LPAREN] ++ [Type'] ++ [RPAREN]
  U.Kind   -> [LPAREN] ++ [Kind'] ++ [RPAREN]
  U.Pi a b -> [LPAREN] ++ [Pi'] ++ splitPreterm a ++ splitPreterm b ++ [RPAREN]
  U.Lam m      -> [LPAREN] ++ [Lam'] ++ splitPreterm m ++ [RPAREN]
  U.App m n    -> [LPAREN] ++ [App'] ++ splitPreterm m ++ splitPreterm n ++ [RPAREN]
  U.Not m  -> [LPAREN] ++ [Not'] ++ splitPreterm m ++ [RPAREN]
  U.Sigma a b  -> [LPAREN] ++ [Sigma'] ++ splitPreterm a ++ splitPreterm b ++ [RPAREN]
  U.Pair m n   -> [LPAREN] ++ [Pair'] ++ splitPreterm m ++ splitPreterm n ++ [RPAREN]
  U.Proj s m   ->
    case s of
      U.Fst -> [LPAREN] ++ [Proj'] ++ [FST] ++ splitPreterm m ++ [RPAREN]
      U.Snd -> [LPAREN] ++ [Proj'] ++ [SND] ++ splitPreterm m ++ [RPAREN]
  U.Disj a b   -> [LPAREN] ++ [Disj'] ++ splitPreterm a ++ splitPreterm b ++ [RPAREN]
  U.Iota s m   ->
    case s of
      U.Fst -> [LPAREN] ++ [Iota'] ++ [FST] ++ splitPreterm m ++ [RPAREN]
      U.Snd -> [LPAREN] ++ [Iota'] ++ [SND] ++ splitPreterm m ++ [RPAREN]
  U.Unpack p h m n -> [LPAREN] ++ [Unpack'] ++ splitPreterm p ++ splitPreterm h ++ splitPreterm m ++ splitPreterm n ++ [RPAREN]
  U.Bot        -> [LPAREN] ++ [Bot'] ++ [RPAREN]
  U.Unit       -> [LPAREN] ++ [Unit'] ++ [RPAREN]
  U.Top        -> [LPAREN] ++ [Top'] ++ [RPAREN]
  U.Entity     -> [LPAREN] ++ [Entity'] ++ [RPAREN]
  U.Nat        -> [LPAREN] ++ [Nat'] ++ [RPAREN]
  U.Zero       -> [LPAREN] ++ [Zero'] ++ [RPAREN]
  U.Succ n     -> [LPAREN] ++ [Succ'] ++ splitPreterm n ++ [RPAREN]
  U.Natrec n e f -> [LPAREN] ++ [Natrec'] ++ splitPreterm n ++ splitPreterm e ++ splitPreterm f ++ [RPAREN]
  U.Eq a m n   -> [LPAREN] ++ [Eq'] ++ splitPreterm a ++ splitPreterm m ++ splitPreterm n ++ [RPAREN]
  U.Refl a m   -> [LPAREN] ++ [Refl'] ++ splitPreterm a ++ splitPreterm m ++ [RPAREN]
  U.Idpeel m n -> [LPAREN] ++ [Idpeel'] ++ splitPreterm m ++ splitPreterm n ++ [RPAREN]


embedPreterm :: U.Preterm -> [Int]
embedPreterm preterm = map fromEnum $ splitPreterm preterm

embedPreterms :: [U.Preterm] -> [Int]
embedPreterms preterms = concatMap (\preterm -> embedPreterm preterm) preterms

-- splitPreterms:: [U.Preterm] -> [IntermediateConstructor]
-- splitPreterms preterms = concatMap (\preterm -> splitPreterm preterm) preterms

embedSignature :: U.Signature -> [Int]
embedSignature signature = concatMap (\(name, preterm) -> [fromEnum LPAREN] ++ [hash name] ++ embedPreterm preterm ++ [fromEnum RPAREN]) signature

-- splitSignature :: U.Signature -> [IntermediateConstructor]
-- splitSignature signature = concatMap (\(name, preterm) -> [LPAREN] ++ splitPreterm preterm ++ [RPAREN]) signature

main :: IO()
main = do
  trainingData <- loadActionsFromBinary saveFilePath

  let judgmentData = map (\(judgment, _) -> (embedSignature $ U.signtr judgment) ++ [fromEnum EOSig] ++ (embedPreterms $ U.contxt judgment) ++ [fromEnum EOCon] ++ (embedPreterm $ U.trm judgment) ++ [fromEnum EOTerm] ++ (embedPreterm $ U.typ judgment) ++ [fromEnum EOTyp]) trainingData
  print judgmentData
  -- let constructorData = map (\(judgment, _) -> (splitSignature $ U.signtr judgment) ++ [EOSig] ++ (splitPreterms $ U.contxt judgment) ++ [EOCon] ++ (splitPreterm $ U.trm judgment) ++ [EOTerm] ++ (splitPreterm $ U.typ judgment) ++ [EOTyp]) trainingData
  -- print constructorData