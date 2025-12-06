module TPTP.Convert (
  processFile,
  evalInfo
) where

import qualified TPTP.Syntax as S
import qualified TPTP.FormulaSyntax as F
import qualified TPTP.Parser as P
import qualified TPTP.FormulaParser as FP
import qualified TPTPInfo as TI

import qualified DTS.DTTdeBruijn as DT
import qualified DTS.Prover.Wani.Arrowterm as A
import qualified Data.Text.Lazy as Te

import Data.Default (Default(..))

-- | TPTPファイルを読み込んでInfoに変換
processFile :: FilePath -> IO TI.Info
processFile filepath = do
  content <- readFile filepath
  case P.parseExpr content of
    Right exprs -> evalInfo exprs filepath
    Left err    -> return $ def{TI.note = "Parse error: " ++ err}

-- | パース済みの式リストをInfoに変換
evalInfo :: [S.Expr] -> FilePath -> IO TI.Info
evalInfo exprs filepath = do
  let initialInfo = def
        { TI.filename = filepath
        , TI.signature = [(Te.pack "entity", DT.Type), (Te.pack "top", DT.Top)]
        , TI.context = [DT.Type]
        , TI.prelst = [("entity", 0)]
        }
  return $ foldl processExpr initialInfo exprs

-- | 個々の式を処理
processExpr :: TI.Info -> S.Expr -> TI.Info
processExpr info expr = case expr of
  
  S.Sout _ -> info
  S.Status s -> info{TI.status = Just (read s)}
  S.PreNum _ -> info
  S.ClaNum _ -> info
  S.Include _ -> info  -- 簡略化（本来はincludeファイルを読む）
  
  S.Formula lang name role formulaStr ->
    let info' = info{TI.language = Just (read lang)}
    in case FP.parseExpr formulaStr of
      Right ast ->
        let knownConsts = filter ((/= "") . fst) $ map fst $ zip (TI.prelst info') [0..]
            (consts, term) = formulaToPreterm ast (map (\(s,i) -> (s,0)) $ TI.prelst info')
            sig' = foldr updateSignature (TI.signature info') consts
            parsedRole = read role :: TI.Role
        in case parsedRole of
          TI.Conjecture ->
            info'{ TI.target = Just term
                 , TI.strtarget = formulaStr
                 , TI.signature = sig'
                 }
          _ | TI.isAxiomLike parsedRole ->
            info'{ TI.context = term : TI.context info'
                 , TI.strcontext = TI.strcontext info' ++ "," ++ formulaStr
                 , TI.signature = sig'
                 }
          _ -> info'
      Left err ->
        info'{TI.note = "Formula parse error: " ++ err ++ " in " ++ formulaStr}
  
  _ -> info

-- | 論理式AST → DT.Preterm
formulaToPreterm :: F.Expr -> [(String, Int)] -> ([(String, Int)], DT.Preterm)
formulaToPreterm expr consts = case expr of
  
  F.Tletter con ->
    let consts' = if con `elem` map fst consts
                  then consts else (con, 0) : consts
    in (consts', DT.Con $ Te.pack con)
  
  F.Ttrue  -> (consts, DT.Top)
  F.Tfalse -> (consts, DT.Bot)
  
  F.Tneg f ->
    let (c', t) = formulaToPreterm f consts
    in (c', DT.Not t)
  
  F.Tbinary op f1 f2 ->
    let (c1, t1) = formulaToPreterm f1 consts
        (c2, t2) = formulaToPreterm f2 c1
    in case op of
      F.Tand   -> (c2, DT.Sigma t1 t2)
      F.Tor    -> (c2, DT.Not $ DT.Sigma (DT.Not t1) (DT.Not t2))
      F.Timp   -> (c2, DT.Pi t1 t2)
      F.Tequiv -> (c2, DT.Sigma (DT.Pi t1 t2) (DT.Pi t2 t1))
      F.Tequal -> (c2, DT.Eq DT.Type t1 t2)
  
  F.TApp f args -> processApp f args consts
  
  F.Tall [] f -> formulaToPreterm f consts
  F.Tall (F.TFormula (F.Tletter var):vars) f ->
    let (c', t) = formulaToPreterm (F.Tall vars f) ((var, 0) : consts)
        c'' = filter ((/= var) . fst) c'
    in (c'', DT.Pi (DT.Con $ Te.pack "entity")
                   (A.subst t (DT.Var 0) (DT.Con $ Te.pack var)))
  F.Tall _ f -> formulaToPreterm f consts
  
  F.Texist [] f -> formulaToPreterm f consts
  F.Texist (F.TFormula (F.Tletter var):vars) f ->
    let (c', t) = formulaToPreterm (F.Texist vars f) ((var, 0) : consts)
        c'' = filter ((/= var) . fst) c'
    in (c'', DT.Sigma (DT.Con $ Te.pack "entity")
                      (A.subst t (DT.Var 0) (DT.Con $ Te.pack var)))
  F.Texist _ f -> formulaToPreterm f consts

-- | 関数適用の処理
processApp :: F.Expr -> [F.Tvar] -> [(String, Int)] -> ([(String, Int)], DT.Preterm)
processApp f [] consts = formulaToPreterm f consts
processApp f (F.TFormula a : rest) consts =
  let (c1, aT)   = formulaToPreterm a consts
      (c2, fApp) = processApp f rest c1
      fName = case f of F.Tletter s -> s; _ -> ""
      arity = length (F.TFormula a : rest)
      c' = updateArity fName arity c2
  in (c', DT.App fApp aT)
processApp f _ consts = formulaToPreterm f consts

-- | 関数のアリティを更新
updateArity :: String -> Int -> [(String, Int)] -> [(String, Int)]
updateArity "" _ c = c
updateArity f arity c =
  case lookup f c of
    Nothing  -> (f, arity) : c
    Just old -> if old < arity
                then (f, arity) : filter ((/= f) . fst) c
                else c

-- | シグネチャを更新
updateSignature :: (String, Int) -> DT.Signature -> DT.Signature
updateSignature (name, arity) sig =
  case lookup (Te.pack name) sig of
    Just _  -> sig
    Nothing -> (Te.pack name, genType arity) : sig

genType :: Int -> DT.Preterm
genType 0 = DT.Type
genType n = DT.Pi (DT.Con $ Te.pack "entity") (genType (n - 1))