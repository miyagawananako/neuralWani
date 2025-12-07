{-# LANGUAGE OverloadedStrings #-}

import qualified DTS.QueryTypes as QT
import qualified Parser.ChartParser as CP
import qualified Parser.PartialParsing as CP
import Parser.LangOptions (defaultJpOptions,defaultEnOptions)
import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
import qualified DTS.DTTdeBruijn as DTT
import System.Environment (setEnv, lookupEnv, getArgs)
import TPTP.Convert (processFile)
import qualified TPTPInfo as TI
import qualified JSeM
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import Text.Printf (printf)
import System.Directory (listDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName, takeBaseName)
import Data.List (isInfixOf, sort, intercalate)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Exception (try, SomeException)
import Text.Read (readMaybe)

-- 証明木関連のインポート
import qualified DTS.Prover.Wani.Prove as Prove   -- prove' 関数
import qualified Interface.Tree as I              -- Tree データ型
import qualified Interface.Text as IText          -- toText
import qualified Interface.TeX as ITeX            -- toTeX
import qualified Interface.HTML as IHTML          -- toMathML, HTML出力
import qualified ListT                            -- ListT モナド

-- | 評価結果を保持するデータ型
data EvalResult = EvalResult
  { erFilename       :: T.Text
  , erExpected       :: Maybe TI.Result
  , erNormalResult   :: TI.Result
  , erNormalTime     :: NominalDiffTime
  , erNeuralResult   :: TI.Result
  , erNeuralTime     :: NominalDiffTime
  , erNormalMatch    :: Bool
  , erNeuralMatch    :: Bool
  } deriving (Show)

-- | スキップされたファイルの情報
data SkippedFile = SkippedFile
  { sfFilename :: T.Text
  , sfReason   :: T.Text
  } deriving (Show)

-- | data/TPTP/ ディレクトリのパス
tptpDir :: FilePath
tptpDir = "data/TPTP"

-- | 対象のサブディレクトリ
targetSubDirs :: [FilePath]
targetSubDirs = ["SYN"]

-- | Proverの設定パラメータ（デフォルト値）
defaultMaxDepth :: Int
defaultMaxDepth = 9

defaultMaxTime :: Int
defaultMaxTime = 6000

-- | Prover設定を保持するデータ型
data ProverConfig = ProverConfig
  { cfgMaxDepth :: Int
  , cfgMaxTime  :: Int
  } deriving (Show)

main :: IO()
main = do
  -- コマンドライン引数からmaxTimeを取得
  args <- getArgs
  let config = case args of
        []        -> ProverConfig defaultMaxDepth defaultMaxTime
        [timeStr] -> case readMaybe timeStr of
          Just t  -> ProverConfig defaultMaxDepth t
          Nothing -> error $ "Invalid maxTime: " ++ timeStr ++ "\nUsage: program [maxTime]"
        _         -> error "Usage: program [maxTime]"
  
  -- 実行開始時刻を取得（レポートと証明木の対応に使用）
  now <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" now
      configStr = "D" ++ show (cfgMaxDepth config) ++ "T" ++ show (cfgMaxTime config)
      sessionId = configStr ++ "_" ++ timestamp
  
  putStrLn $ "=== Prover Configuration ==="
  putStrLn $ "maxDepth: " ++ show (cfgMaxDepth config)
  putStrLn $ "maxTime:  " ++ show (cfgMaxTime config)
  putStrLn $ "Session:  " ++ sessionId
  putStrLn ""
  
  -- ========================================
  -- 単一ファイルを指定する場合はこちらをコメント解除
  -- ========================================
  -- let singleFile = ("SYN", "SYN000+1.p")
  -- putStrLn $ "Processing single file: " ++ show singleFile
  -- result <- processOneFile config sessionId singleFile
  -- case result of
  --   Left skipped -> putStrLn $ "Skipped: " ++ T.unpack (sfReason skipped)
  --   Right eval   -> writeTexReport config sessionId [eval] []
  
  -- ========================================
  -- 全FOFファイルを処理する場合はこちら（デフォルト）
  -- ========================================
  -- data/TPTP/ 配下のサブディレクトリからファイルを取得
  -- fofFilesWithSubDir <- fmap concat $ mapM getFilesFromSubDir targetSubDirs
  
  -- let fofFiles = sort fofFilesWithSubDir
  let fofFiles = [("SYN", "SYN950+1.p"), ("SYN", "SYN952+1.p"), ("SYN", "SYN958+1.p")]
  
  -- Found 376 FOF files (with '+' in filename)！！
  putStrLn $ "Found " ++ show (length fofFiles) ++ " FOF files (with '+' in filename)"
  putStrLn ""
  
  -- 各ファイルを処理して結果を集計
  results <- mapM (processOneFile config sessionId) fofFiles
  
  -- 成功した結果とスキップされたファイルを分離
  let (skipped, evaluated) = partitionResults results
  
  -- TeX形式でレポートを出力
  writeTexReport config sessionId evaluated skipped
  
  -- サマリーを表示
  printSummary evaluated skipped

-- | 結果を分離する
partitionResults :: [Either SkippedFile EvalResult] -> ([SkippedFile], [EvalResult])
partitionResults = foldr f ([], [])
  where
    f (Left s)  (ss, es) = (s:ss, es)
    f (Right e) (ss, es) = (ss, e:es)

-- | サブディレクトリからFOFファイル（"+"を含む）を取得
getFilesFromSubDir :: FilePath -> IO [(FilePath, FilePath)]
getFilesFromSubDir subDir = do
  let dirPath = tptpDir </> subDir
  allFiles <- listDirectory dirPath
  -- ファイル名に "+" が含まれているものだけをフィルタリング（FOF形式）
  let fofFiles = filter ('+' `elem`) allFiles
  return $ map (\f -> (subDir, f)) fofFiles

-- | 1つのファイルを処理する
processOneFile :: ProverConfig -> String -> (FilePath, FilePath) -> IO (Either SkippedFile EvalResult)
processOneFile config sessionId (subDir, filename) = do
  let filepath = tptpDir </> subDir </> filename
      filenameText = T.pack (subDir </> filename)
  info <- processFile filepath
  
  -- TPTPファイルから取得したデータを変数に格納
  let context   :: [DTT.Preterm]
      context   = TI.context info
      
      target    :: Maybe DTT.Preterm
      target    = TI.target info
      
      signature :: DTT.Signature
      signature = TI.signature info
      
      status    :: Maybe TI.Status
      status    = TI.status info
      
      language  :: Maybe TI.Language
      language  = TI.language info
  
  -- 必要なデータが取得できているか確認
  let errors = concat
        [ ["target is Nothing"   | Nothing <- [target]]
        , ["status is Nothing"   | Nothing <- [status]]
        , ["language is Nothing" | Nothing <- [language]]
        , ["language is not FOF (" ++ show language ++ ")" | Just l <- [language], l /= TI.FOF]
        ]
  
  if not (null errors)
    then do
      putStrLn $ "=== File: " ++ subDir </> filename ++ " ==="
      putStrLn $ "Error: " ++ intercalate ", " errors
      return $ Left SkippedFile
        { sfFilename = filenameText
        , sfReason   = T.pack $ intercalate ", " errors
        }
    else do
      let Just t    = target
          Just st   = status
          Just lang = language
      -- 全てのデータが正しく取得できた
      putStrLn $ "=== File: " ++ subDir </> filename ++ " ==="
      putStrLn $ "  context:   " ++ show (length context) ++ " axioms"
      putStrLn $ "  target:    OK"
      putStrLn $ "  signature: " ++ show (length signature) ++ " entries"
      putStrLn $ "  status:    " ++ show st
      putStrLn $ "  language:  " ++ show lang
      
      -- Proverの設定（コマンドライン引数から取得した値を使用）
      let normalSetting = QT.defaultProofSearchSetting {
                QT.maxDepth = Just (cfgMaxDepth config),
                QT.maxTime = Just (cfgMaxTime config)
                }
      -- NeuralWani用の設定（将来的にニューラルネットワークを有効化）
      let neuralSetting = QT.defaultProofSearchSetting {
                QT.maxDepth = Just (cfgMaxDepth config),
                QT.maxTime = Just (cfgMaxTime config)
                -- QT.neuralWani = Just getPrioritizedRules  -- 将来の実装用
                }
      
      -- 期待される結果（StatusからResultに変換）
      let expectedResult :: Maybe TI.Result
          expectedResult = TI.statusToResult <$> TI.status info
      
      -- 証明木出力用のディレクトリとベース名
      let baseName = takeBaseName filename
          treeBaseDir = "evaluateResult" </> ("proofTrees_" ++ sessionId)
      
      -- ========================================
      -- Normal Prover での証明探索
      -- ========================================
      putStrLn ""
      putStrLn "=== Normal Prover ==="
      (normalTree, normalTime) <- runProveWithTree normalSetting signature context t
      let normalResult = proofTreeToResult normalTree
      putStrLn $ "  Result: " ++ show normalResult
      putStrLn $ "  Time:   " ++ T.unpack (formatTimeNominal normalTime)
      
      -- 証明木をファイルに出力（normal/ サブディレクトリに保存）
      let normalTreeDir = treeBaseDir </> "normal"
      writeProofTrees normalTreeDir baseName normalTree
      
      -- 期待値との比較
      let normalMatch = maybe False (== normalResult) expectedResult
      printMatchResult normalMatch expectedResult normalResult
      
      -- ========================================
      -- NeuralWani Prover での証明探索
      -- ========================================
      putStrLn ""
      putStrLn "=== NeuralWani Prover ==="
      (neuralTree, neuralTime) <- runProveWithTree neuralSetting signature context t
      let neuralResult = proofTreeToResult neuralTree
      putStrLn $ "  Result: " ++ show neuralResult
      putStrLn $ "  Time:   " ++ T.unpack (formatTimeNominal neuralTime)
      
      -- 証明木をファイルに出力（neural/ サブディレクトリに保存）
      let neuralTreeDir = treeBaseDir </> "neural"
      writeProofTrees neuralTreeDir baseName neuralTree
      
      -- 期待値との比較
      let neuralMatch = maybe False (== neuralResult) expectedResult
      printMatchResult neuralMatch expectedResult neuralResult
      
      -- ========================================
      -- 比較結果
      -- ========================================
      putStrLn ""
      putStrLn "=== Comparison ==="
      putStrLn $ "Normal time:     " ++ T.unpack (formatTimeNominal normalTime)
      putStrLn $ "NeuralWani time: " ++ T.unpack (formatTimeNominal neuralTime)
      let speedup = if neuralTime > 0 
                    then realToFrac normalTime / realToFrac neuralTime :: Double
                    else 0
      printf "Speedup: %.2fx\n" speedup
      putStrLn $ "Results match: " ++ show (normalResult == neuralResult)
      
      putStrLn ""
      putStrLn "=========================================="
      putStrLn ""
      
      return $ Right EvalResult
        { erFilename     = filenameText
        , erExpected     = expectedResult
        , erNormalResult = normalResult
        , erNormalTime   = normalTime
        , erNeuralResult = neuralResult
        , erNeuralTime   = neuralTime
        , erNormalMatch  = normalMatch
        , erNeuralMatch  = neuralMatch
        }

-- | 期待値との比較結果を表示する
printMatchResult :: Bool -> Maybe TI.Result -> TI.Result -> IO ()
printMatchResult match expectedResult actualResult = 
  case expectedResult of
    Just expected ->
      if match
        then putStrLn $ "  ✓ MATCH"
        else putStrLn $ "  ✗ MISMATCH: got " ++ show actualResult ++ ", expected " ++ show expected
    Nothing -> putStrLn "  ? Cannot compare (no expected status)"

-- | 時間をフォーマットする
formatTimeNominal :: NominalDiffTime -> T.Text
formatTimeNominal t = T.pack $ printf "%.3f sec" (realToFrac t :: Double)

-- | サマリーを表示する
printSummary :: [EvalResult] -> [SkippedFile] -> IO ()
printSummary results skipped = do
  putStrLn ""
  putStrLn "============================================"
  putStrLn "=== SUMMARY ==="
  putStrLn "============================================"
  putStrLn $ "Total files processed: " ++ show (length results + length skipped)
  putStrLn $ "Evaluated: " ++ show (length results)
  putStrLn $ "Skipped:   " ++ show (length skipped)
  putStrLn ""
  
  let normalMatches = length $ filter erNormalMatch results
      neuralMatches = length $ filter erNeuralMatch results
      totalEval = length results
  
  putStrLn "--- Normal Prover ---"
  putStrLn $ "Correct: " ++ show normalMatches ++ "/" ++ show totalEval
  printf "Accuracy: %.1f%%\n" (100.0 * fromIntegral normalMatches / fromIntegral totalEval :: Double)
  
  putStrLn ""
  putStrLn "--- NeuralWani Prover ---"
  putStrLn $ "Correct: " ++ show neuralMatches ++ "/" ++ show totalEval
  printf "Accuracy: %.1f%%\n" (100.0 * fromIntegral neuralMatches / fromIntegral totalEval :: Double)
  
  -- 平均時間
  let avgNormalTime = sum (map erNormalTime results) / fromIntegral totalEval
      avgNeuralTime = sum (map erNeuralTime results) / fromIntegral totalEval
  putStrLn ""
  putStrLn "--- Average Time ---"
  putStrLn $ "Normal:     " ++ T.unpack (formatTimeNominal avgNormalTime)
  putStrLn $ "NeuralWani: " ++ T.unpack (formatTimeNominal avgNeuralTime)

-- | TeX形式でレポートを出力する
writeTexReport :: ProverConfig -> String -> [EvalResult] -> [SkippedFile] -> IO ()
writeTexReport config sessionId results skipped = do
  -- 出力ディレクトリを作成
  createDirectoryIfMissing True "evaluateResult"
  
  -- sessionIdを使用してファイル名を決定（証明木ディレクトリと対応）
  let texFilename = "evaluateResult" </> ("report_" ++ sessionId ++ ".tex")
      proofTreeDirName = "proofTrees_" ++ sessionId
  
  -- TeXファイルを書き込み
  T.writeFile texFilename $ generateTexContent config sessionId results skipped
  
  putStrLn ""
  putStrLn $ "TeX report written to: " ++ texFilename
  putStrLn $ "Proof trees saved to:  evaluateResult/" ++ proofTreeDirName ++ "/normal/"
  putStrLn $ "                       evaluateResult/" ++ proofTreeDirName ++ "/neural/"

-- | TeXコンテンツを生成する
generateTexContent :: ProverConfig -> String -> [EvalResult] -> [SkippedFile] -> T.Text
generateTexContent config sessionId results skipped = T.unlines
  [ "\\documentclass[a4paper,10pt]{article}"
  , "\\usepackage[utf8]{inputenc}"
  , "\\usepackage{booktabs}"
  , "\\usepackage{longtable}"
  , "\\usepackage{geometry}"
  , "\\usepackage{xcolor}"
  , "\\usepackage{colortbl}"
  , "\\geometry{margin=1.5cm}"
  , ""
  , "\\definecolor{matchcolor}{RGB}{200,255,200}"
  , "\\definecolor{mismatchcolor}{RGB}{255,200,200}"
  , ""
  , "\\title{TPTP Evaluation Report}"
  , "\\author{NeuralWani Evaluator}"
  , "\\date{\\today}"
  , ""
  , "\\begin{document}"
  , "\\maketitle"
  , ""
  , "\\section{Configuration}"
  , "\\begin{itemize}"
  , "\\item maxDepth: " <> T.pack (show (cfgMaxDepth config))
  , "\\item maxTime: " <> T.pack (show (cfgMaxTime config))
  , "\\item Session ID: \\texttt{" <> escapeTeX (T.pack sessionId) <> "}"
  , "\\item Proof Trees (Normal): \\texttt{evaluateResult/proofTrees\\_" <> escapeTeX (T.pack sessionId) <> "/normal/}"
  , "\\item Proof Trees (NeuralWani): \\texttt{evaluateResult/proofTrees\\_" <> escapeTeX (T.pack sessionId) <> "/neural/}"
  , "\\end{itemize}"
  , ""
  , "\\section{Summary}"
  , ""
  , generateSummaryTex results skipped
  , ""
  , "\\section{Detailed Results}"
  , ""
  , generateResultsTable results
  , ""
  , if null skipped then "" else generateSkippedSection skipped
  , ""
  , "\\end{document}"
  ]

-- | サマリーのTeXを生成
generateSummaryTex :: [EvalResult] -> [SkippedFile] -> T.Text
generateSummaryTex results skipped = T.unlines
  [ "\\begin{tabular}{ll}"
  , "\\toprule"
  , "\\textbf{Metric} & \\textbf{Value} \\\\"
  , "\\midrule"
  , "Total files processed & " <> T.pack (show (length results + length skipped)) <> " \\\\"
  , "Evaluated & " <> T.pack (show (length results)) <> " \\\\"
  , "Skipped & " <> T.pack (show (length skipped)) <> " \\\\"
  , "\\midrule"
  , "Normal Prover Correct & " <> T.pack (show normalMatches) <> "/" <> T.pack (show totalEval) 
      <> " (" <> T.pack (printf "%.1f" normalAcc) <> "\\%) \\\\"
  , "NeuralWani Prover Correct & " <> T.pack (show neuralMatches) <> "/" <> T.pack (show totalEval) 
      <> " (" <> T.pack (printf "%.1f" neuralAcc) <> "\\%) \\\\"
  , "\\midrule"
  , "Avg. Normal Time & " <> formatTimeNominal avgNormalTime <> " \\\\"
  , "Avg. NeuralWani Time & " <> formatTimeNominal avgNeuralTime <> " \\\\"
  , "\\bottomrule"
  , "\\end{tabular}"
  , ""
  , "\\subsection{Confusion Matrix (Normal Prover)}"
  , ""
  , generateConfusionMatrix results erNormalResult
  , ""
  , "\\subsection{Confusion Matrix (NeuralWani Prover)}"
  , ""
  , generateConfusionMatrix results erNeuralResult
  ]
  where
    normalMatches = length $ filter erNormalMatch results
    neuralMatches = length $ filter erNeuralMatch results
    totalEval = length results
    normalAcc = if totalEval == 0 then 0 else 100.0 * fromIntegral normalMatches / fromIntegral totalEval :: Double
    neuralAcc = if totalEval == 0 then 0 else 100.0 * fromIntegral neuralMatches / fromIntegral totalEval :: Double
    avgNormalTime = if totalEval == 0 then 0 else sum (map erNormalTime results) / fromIntegral totalEval
    avgNeuralTime = if totalEval == 0 then 0 else sum (map erNeuralTime results) / fromIntegral totalEval

-- | 混同行列のTeXを生成（predicted vs actual）
generateConfusionMatrix :: [EvalResult] -> (EvalResult -> TI.Result) -> T.Text
generateConfusionMatrix results getResult = T.unlines
  [ "\\begin{tabular}{l|ccc|c}"
  , "\\toprule"
  , " & \\multicolumn{3}{c|}{\\textbf{Predicted}} & \\\\"
  , "\\textbf{Actual} & YES & NO & UNK & Total \\\\"
  , "\\midrule"
  , "YES & " <> T.pack (show yy) <> " & " <> T.pack (show yn) <> " & " <> T.pack (show yu) <> " & " <> T.pack (show (yy + yn + yu)) <> " \\\\"
  , "NO & " <> T.pack (show ny) <> " & " <> T.pack (show nn) <> " & " <> T.pack (show nu) <> " & " <> T.pack (show (ny + nn + nu)) <> " \\\\"
  , "UNK & " <> T.pack (show uy) <> " & " <> T.pack (show un) <> " & " <> T.pack (show uu) <> " & " <> T.pack (show (uy + un + uu)) <> " \\\\"
  , "\\midrule"
  , "Total & " <> T.pack (show (yy + ny + uy)) <> " & " <> T.pack (show (yn + nn + un)) <> " & " <> T.pack (show (yu + nu + uu)) <> " & " <> T.pack (show total) <> " \\\\"
  , "\\bottomrule"
  , "\\end{tabular}"
  ]
  where
    -- 期待値（actual）がある結果のみを対象にする
    resultsWithExpected = filter (isJust . erExpected) results
    
    -- カウント関数
    countPair :: TI.Result -> TI.Result -> Int
    countPair actual predicted = length $ filter match resultsWithExpected
      where
        match r = erExpected r == Just actual && getResult r == predicted
    
    -- YES (actual) の行
    yy = countPair TI.YES TI.YES
    yn = countPair TI.YES TI.NO
    yu = countPair TI.YES TI.UNKNOWN
    
    -- NO (actual) の行
    ny = countPair TI.NO TI.YES
    nn = countPair TI.NO TI.NO
    nu = countPair TI.NO TI.UNKNOWN
    
    -- UNKNOWN (actual) の行
    uy = countPair TI.UNKNOWN TI.YES
    un = countPair TI.UNKNOWN TI.NO
    uu = countPair TI.UNKNOWN TI.UNKNOWN
    
    total = length resultsWithExpected
    
    isJust (Just _) = True
    isJust Nothing  = False

-- | 結果テーブルのTeXを生成
generateResultsTable :: [EvalResult] -> T.Text
generateResultsTable results = T.unlines $
  [ "\\begin{longtable}{lcccccccc}"
  , "\\toprule"
  , "\\textbf{File} & \\textbf{Expected} & \\textbf{Normal} & \\textbf{Time} & \\textbf{Match} & \\textbf{Neural} & \\textbf{Time} & \\textbf{Match} & \\textbf{Speedup} \\\\"
  , "\\midrule"
  , "\\endhead"
  ] ++ map resultToTexRow results ++
  [ "\\bottomrule"
  , "\\end{longtable}"
  ]

-- | 1行分のTeXを生成
resultToTexRow :: EvalResult -> T.Text
resultToTexRow r = T.concat
  [ escapeTeX (erFilename r), " & "
  , maybe "-" (T.pack . show) (erExpected r), " & "
  , T.pack (show (erNormalResult r)), " & "
  , formatTimeShort (erNormalTime r), " & "
  , matchSymbol (erNormalMatch r), " & "
  , T.pack (show (erNeuralResult r)), " & "
  , formatTimeShort (erNeuralTime r), " & "
  , matchSymbol (erNeuralMatch r), " & "
  , T.pack (printf "%.2fx" speedup)
  , " \\\\"
  ]
  where
    speedup = realToFrac (erNormalTime r) / realToFrac (erNeuralTime r) :: Double
    matchSymbol True  = "\\cellcolor{matchcolor}$\\checkmark$"
    matchSymbol False = "\\cellcolor{mismatchcolor}$\\times$"

-- | 短い時間フォーマット
formatTimeShort :: NominalDiffTime -> T.Text
formatTimeShort t = T.pack $ printf "%.3fs" (realToFrac t :: Double)

-- | スキップされたファイルのセクションを生成
generateSkippedSection :: [SkippedFile] -> T.Text
generateSkippedSection skipped = T.unlines $
  [ "\\section{Skipped Files}"
  , ""
  , "\\begin{longtable}{ll}"
  , "\\toprule"
  , "\\textbf{File} & \\textbf{Reason} \\\\"
  , "\\midrule"
  , "\\endhead"
  ] ++ map skippedToTexRow skipped ++
  [ "\\bottomrule"
  , "\\end{longtable}"
  ]

-- | スキップファイルの行を生成
skippedToTexRow :: SkippedFile -> T.Text
skippedToTexRow s = escapeTeX (sfFilename s) <> " & " <> escapeTeX (sfReason s) <> " \\\\"

-- | TeXの特殊文字をエスケープ
escapeTeX :: T.Text -> T.Text
escapeTeX = T.concatMap escapeChar
  where
    escapeChar '_' = "\\_"
    escapeChar '#' = "\\#"
    escapeChar '%' = "\\%"
    escapeChar '&' = "\\&"
    escapeChar '$' = "\\$"
    escapeChar '{' = "\\{"
    escapeChar '}' = "\\}"
    escapeChar '^' = "\\^{}"
    escapeChar '~' = "\\~{}"
    escapeChar c   = T.singleton c

-- ============================================
-- 証明木出力関連の関数
-- ============================================

-- | prove' を使って証明探索を実行し、証明木を取得する
runProveWithTree :: QT.ProofSearchSetting -> DTT.Signature -> [DTT.Preterm] -> DTT.Preterm
                 -> IO (Maybe (I.Tree QT.DTTrule DTT.Judgment), NominalDiffTime)
runProveWithTree setting sig ctx targetType = do
  startTime <- getCurrentTime
  
  -- ProofSearchQueryを構築
  let query = DTT.ProofSearchQuery sig ctx targetType
  
  -- prove' を使用して証明木を取得
  let prover = Prove.prove' setting
  trees <- ListT.toList (prover query)
  
  endTime <- getCurrentTime
  let elapsedTime = diffUTCTime endTime startTime
  
  -- 最初の証明木を返す（存在すれば）
  case trees of
    (tree:_) -> return (Just tree, elapsedTime)
    []       -> return (Nothing, elapsedTime)

-- | 証明木の結果をTI.Resultに変換
proofTreeToResult :: Maybe (I.Tree QT.DTTrule DTT.Judgment) -> TI.Result
proofTreeToResult (Just _) = TI.YES
proofTreeToResult Nothing  = TI.UNKNOWN

-- | 証明木をテキストファイルに出力
writeProofTreeText :: FilePath -> I.Tree QT.DTTrule DTT.Judgment -> IO ()
writeProofTreeText filepath tree = do
  let content = IText.toText tree
  T.writeFile filepath content

-- | 証明木をTeX形式でファイルに出力
writeProofTreeTeX :: FilePath -> I.Tree QT.DTTrule DTT.Judgment -> IO ()
writeProofTreeTeX filepath tree = do
  let treeTeX = ITeX.toTeX tree
      content = T.unlines
        [ "\\documentclass[a4paper,10pt]{article}"
        , "\\usepackage[utf8]{inputenc}"
        , "\\usepackage{amsmath}"
        , "\\usepackage{amssymb}"
        , "\\usepackage{bussproofs}"
        , "\\usepackage{geometry}"
        , "\\geometry{margin=1cm}"
        , ""
        , "% Natural Deduction マクロ"
        , "\\newcommand{\\nd}[3]{\\AxiomC{#3}\\RightLabel{\\scriptsize #1}\\UnaryInfC{#2}}"
        , ""
        , "\\begin{document}"
        , ""
        , "\\section*{Proof Tree}"
        , ""
        , "\\begin{prooftree}"
        , treeTeX
        , "\\end{prooftree}"
        , ""
        , "\\end{document}"
        ]
  T.writeFile filepath content

-- | 証明木をHTML（MathML）形式でファイルに出力
writeProofTreeHTML :: FilePath -> I.Tree QT.DTTrule DTT.Judgment -> IO ()
writeProofTreeHTML filepath tree = do
  content <- Prove.display tree
  T.writeFile filepath content

-- | 証明木を複数の形式で出力
writeProofTrees :: FilePath -> String -> Maybe (I.Tree QT.DTTrule DTT.Judgment) -> IO ()
writeProofTrees baseDir baseName maybeTree = do
  createDirectoryIfMissing True baseDir
  case maybeTree of
    Nothing -> do
      -- 証明が見つからなかった場合
      let noProofFile = baseDir </> (baseName ++ "_no_proof.txt")
      writeFile noProofFile "No proof found."
    Just tree -> do
      -- テキスト形式
      let textFile = baseDir </> (baseName ++ ".txt")
      writeProofTreeText textFile tree
      putStrLn $ "  Proof tree (text): " ++ textFile
      
      -- HTML形式（MathML）
      let htmlFile = baseDir </> (baseName ++ ".html")
      writeProofTreeHTML htmlFile tree
      putStrLn $ "  Proof tree (HTML): " ++ htmlFile