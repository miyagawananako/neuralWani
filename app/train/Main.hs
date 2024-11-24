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

data IntermediateConstructor = EOSig | EOCon | EOTerm | EOTyp | FST | SND | LPAREN | RPAREN | OTHER
                              | Sig'p | Sig'q | Sig'r | Sig'A | Sig'B | Sig'C | Sig'a | Sig'entity | Sig'girl | Sig'man | Sig'x | Sig'thesis | Sig'write | Sig'enter | Sig'whistle | Sig'love | Sig'y | Sig'f | Sig'sister | Sig'family | Sig'z | Sig'dog
                              | Var'0 | Var'1 | Var'2 | Var'3 | Var'4 | Var'5 | Var'6
                              | Con'forwardEqIntro | Con'eqElim | Con'dne | Con'efq | Con'p | Con'q | Con'r | Con'A | Con'B | Con'C | Con'a | Con'entity | Con'girl | Con'man | Con'x | Con'thesis | Con'write | Con'enter | Con'whistle | Con'love | Con'y | Con'f | Con'sister | Con'family | Con'z | Con'dog
                              | Type' | Kind' | Pi' | Lam' | App' | Not' | Sigma' | Pair' | Proj' | Disj' | Iota' | Unpack' | Bot' | Unit' | Top' | Entity' | Nat' | Zero' | Succ' | Natrec' | Eq' | Refl' | Idpeel'
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
  U.Con c  ->
    case () of
      _ | c == T.pack "forwardEqIntro" -> [LPAREN] ++ [Con'forwardEqIntro] ++ [RPAREN]
        | c == T.pack "eqElim"         -> [LPAREN] ++ [Con'eqElim] ++ [RPAREN]
        | c == T.pack " dne "          -> [LPAREN] ++ [Con'dne] ++ [RPAREN]
        | c == T.pack " efq "          -> [LPAREN] ++ [Con'efq] ++ [RPAREN]  -- 該当するものがなかった
        | c == T.pack "p"              -> [LPAREN] ++ [Con'p] ++ [RPAREN]
        | c == T.pack "q"              -> [LPAREN] ++ [Con'q] ++ [RPAREN]
        | c == T.pack "r"              -> [LPAREN] ++ [Con'r] ++ [RPAREN]
        | c == T.pack "A"              -> [LPAREN] ++ [Con'A] ++ [RPAREN]
        | c == T.pack "B"              -> [LPAREN] ++ [Con'B] ++ [RPAREN]
        | c == T.pack "C"              -> [LPAREN] ++ [Con'C] ++ [RPAREN]
        | c == T.pack "a"              -> [LPAREN] ++ [Con'a] ++ [RPAREN]
        | c == T.pack "entity"         -> [LPAREN] ++ [Con'entity] ++ [RPAREN]
        | c == T.pack "girl"           -> [LPAREN] ++ [Con'girl] ++ [RPAREN]
        | c == T.pack "man"            -> [LPAREN] ++ [Con'man] ++ [RPAREN]
        | c == T.pack "x"              -> [LPAREN] ++ [Con'x] ++ [RPAREN]
        | c == T.pack "thesis"         -> [LPAREN] ++ [Con'thesis] ++ [RPAREN]
        | c == T.pack "write"          -> [LPAREN] ++ [Con'write] ++ [RPAREN]
        | c == T.pack "enter"          -> [LPAREN] ++ [Con'enter] ++ [RPAREN]
        | c == T.pack "whistle"        -> [LPAREN] ++ [Con'whistle] ++ [RPAREN]
        | c == T.pack "love"           -> [LPAREN] ++ [Con'love] ++ [RPAREN]
        | c == T.pack "y"              -> [LPAREN] ++ [Con'y] ++ [RPAREN]
        | c == T.pack "f"              -> [LPAREN] ++ [Con'f] ++ [RPAREN]
        | c == T.pack "sister"         -> [LPAREN] ++ [Con'sister] ++ [RPAREN]
        | c == T.pack "family"         -> [LPAREN] ++ [Con'family] ++ [RPAREN]
        | c == T.pack "z"              -> [LPAREN] ++ [Con'z] ++ [RPAREN]
        | c == T.pack "dog"            -> [LPAREN] ++ [Con'dog] ++ [RPAREN]
        | otherwise                    -> [LPAREN] ++ [OTHER] ++ [RPAREN]
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


splitPreterms:: [U.Preterm] -> [IntermediateConstructor]
splitPreterms preterms = concatMap (\preterm -> splitPreterm preterm) preterms

splitSignature :: U.Signature -> [IntermediateConstructor]
splitSignature signature = concatMap (\(name, preterm) ->
  case () of
    _ | name == T.pack "p"      -> [LPAREN] ++ [Sig'p] ++ splitPreterm preterm ++ [RPAREN]
      | name == T.pack "q"      -> [LPAREN] ++ [Sig'q] ++ splitPreterm preterm ++ [RPAREN]
      | name == T.pack "r"      -> [LPAREN] ++ [Sig'r] ++ splitPreterm preterm ++ [RPAREN]
      | name == T.pack "A"      -> [LPAREN] ++ [Sig'A] ++ splitPreterm preterm ++ [RPAREN]
      | name == T.pack "B"      -> [LPAREN] ++ [Sig'B] ++ splitPreterm preterm ++ [RPAREN]
      | name == T.pack "C"      -> [LPAREN] ++ [Sig'C] ++ splitPreterm preterm ++ [RPAREN]
      | name == T.pack "a"      -> [LPAREN] ++ [Sig'a] ++ splitPreterm preterm ++ [RPAREN]
      | name == T.pack "entity" -> [LPAREN] ++ [Sig'entity] ++ splitPreterm preterm ++ [RPAREN]
      | name == T.pack "girl"   -> [LPAREN] ++ [Sig'girl] ++ splitPreterm preterm ++ [RPAREN]
      | name == T.pack "man"    -> [LPAREN] ++ [Sig'man] ++ splitPreterm preterm ++ [RPAREN]
      | name == T.pack "x"      -> [LPAREN] ++ [Sig'x] ++ splitPreterm preterm ++ [RPAREN]
      | name == T.pack "thesis" -> [LPAREN] ++ [Sig'thesis] ++ splitPreterm preterm ++ [RPAREN]
      | name == T.pack "write"  -> [LPAREN] ++ [Sig'write] ++ splitPreterm preterm ++ [RPAREN]
      | name == T.pack "enter"  -> [LPAREN] ++ [Sig'enter] ++ splitPreterm preterm ++ [RPAREN]
      | name == T.pack "whistle"-> [LPAREN] ++ [Sig'whistle] ++ splitPreterm preterm ++ [RPAREN]
      | name == T.pack "love"   -> [LPAREN] ++ [Sig'love] ++ splitPreterm preterm ++ [RPAREN]
      | name == T.pack "y"      -> [LPAREN] ++ [Sig'y] ++ splitPreterm preterm ++ [RPAREN]
      | name == T.pack "f"      -> [LPAREN] ++ [Sig'f] ++ splitPreterm preterm ++ [RPAREN]
      | name == T.pack "sister" -> [LPAREN] ++ [Sig'sister] ++ splitPreterm preterm ++ [RPAREN]
      | name == T.pack "family" -> [LPAREN] ++ [Sig'family] ++ splitPreterm preterm ++ [RPAREN]
      | name == T.pack "z"      -> [LPAREN] ++ [Sig'z] ++ splitPreterm preterm ++ [RPAREN]
      | name == T.pack "dog"    -> [LPAREN] ++ [Sig'dog] ++ splitPreterm preterm ++ [RPAREN]
      | otherwise               -> [LPAREN] ++ [OTHER] ++ splitPreterm preterm ++ [RPAREN]  -- 2つだけ該当する
  ) signature

splitJudgment :: U.Judgment -> [IntermediateConstructor]
splitJudgment judgment = (splitSignature $ U.signtr judgment) ++ [EOSig] ++ (splitPreterms $ U.contxt judgment) ++ [EOCon] ++ (splitPreterm $ U.trm judgment) ++ [EOTerm] ++ (splitPreterm $ U.typ judgment) ++ [EOTyp]

embed :: [IntermediateConstructor] -> [Int]
embed = map fromEnum

main :: IO()
main = do
  trainingData <- loadActionsFromBinary saveFilePath
  print trainingData

  let constructorData = map (\(judgment, _) -> splitJudgment judgment) trainingData
  print constructorData
  let embeddedData = map (\judgment -> embed judgment) constructorData
  print embeddedData