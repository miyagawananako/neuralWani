{-# LANGUAGE OverloadedStrings #-}

import qualified DTS.NaturalLanguageInference as NLI
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
import System.FilePath ((</>), takeFileName)
import Data.List (isInfixOf, sort, intercalate)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Exception (try, SomeException)
import Text.Read (readMaybe)

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

-- | data/SYN/ ディレクトリのパス
synDir :: FilePath
synDir = "data/SYN"

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
  
  putStrLn $ "=== Prover Configuration ==="
  putStrLn $ "maxDepth: " ++ show (cfgMaxDepth config)
  putStrLn $ "maxTime:  " ++ show (cfgMaxTime config)
  putStrLn ""
  
  -- ========================================
  -- 単一ファイルを指定する場合はこちらをコメント解除
  -- ========================================
  -- let singleFile = "SYN000+1.p"
  -- putStrLn $ "Processing single file: " ++ singleFile
  -- result <- processOneFile config singleFile
  -- case result of
  --   Left skipped -> putStrLn $ "Skipped: " ++ sfReason skipped
  --   Right eval   -> writeTexReport config [eval] []
  
  -- ========================================
  -- 全FOFファイルを処理する場合はこちら（デフォルト）
  -- ========================================
  -- data/SYN/ ディレクトリ内のファイルを取得
  allFiles <- listDirectory synDir
  
  -- ファイル名に "+" が含まれているものだけをフィルタリング（FOF形式）
  let fofFiles = sort $ filter ('+' `elem`) allFiles
  -- let fofFiles = ["SYN950+1.p", "SYN952+1.p", "SYN958+1.p"]
  
  -- Found 376 FOF files (with '+' in filename)！！
  putStrLn $ "Found " ++ show (length fofFiles) ++ " FOF files (with '+' in filename)"
  putStrLn ""
  
  -- 各ファイルを処理して結果を集計
  results <- mapM (processOneFile config) fofFiles
  
  -- 成功した結果とスキップされたファイルを分離
  let (skipped, evaluated) = partitionResults results
  
  -- TeX形式でレポートを出力
  writeTexReport config evaluated skipped
  
  -- サマリーを表示
  printSummary evaluated skipped

-- | 結果を分離する
partitionResults :: [Either SkippedFile EvalResult] -> ([SkippedFile], [EvalResult])
partitionResults = foldr f ([], [])
  where
    f (Left s)  (ss, es) = (s:ss, es)
    f (Right e) (ss, es) = (ss, e:es)

-- | 1つのファイルを処理する
processOneFile :: ProverConfig -> FilePath -> IO (Either SkippedFile EvalResult)
processOneFile config filename = do
  let filepath = synDir </> filename
      filenameText = T.pack filename
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
      putStrLn $ "=== File: " ++ filename ++ " ==="
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
      putStrLn $ "=== File: " ++ filename ++ " ==="
      putStrLn $ "  context:   " ++ show (length context) ++ " axioms"
      putStrLn $ "  target:    OK"
      putStrLn $ "  signature: " ++ show (length signature) ++ " entries"
      putStrLn $ "  status:    " ++ show st
      putStrLn $ "  language:  " ++ show lang
      
      -- Proverの設定（コマンドライン引数から取得した値を使用）
      let normalProver = NLI.getProver NLI.Wani $ QT.defaultProofSearchSetting {
                QT.maxDepth = Just (cfgMaxDepth config),
                QT.maxTime = Just (cfgMaxTime config)
                }
      -- getPrioritizedRules <- neuralWaniBuilder  -- srcから引っ張ってくる
      let neuralWaniProver = NLI.getProver NLI.Wani $ QT.defaultProofSearchSetting {
                QT.maxDepth = Just (cfgMaxDepth config),
                QT.maxTime = Just (cfgMaxTime config)
                -- QT.neuralWani = Just getPrioritizedRules
                }
      
      -- 期待される結果（StatusからResultに変換）
      let expectedResult :: Maybe TI.Result
          expectedResult = TI.statusToResult <$> TI.status info
      
      putStrLn ""
      putStrLn "=========================================="
      putStrLn "=== Normal Prover ==="
      (normalResult, normalTime) <- runProverWithTime normalProver signature context t
      printProverResult "Normal" normalResult normalTime expectedResult
      
      putStrLn ""
      putStrLn "=== NeuralWani Prover ==="
      (neuralResult, neuralTime) <- runProverWithTime neuralWaniProver signature context t
      printProverResult "NeuralWani" neuralResult neuralTime expectedResult
      
      -- 比較結果
      putStrLn ""
      putStrLn "=== Comparison ==="
      putStrLn $ "Normal time:     " ++ T.unpack (formatTimeNominal normalTime)
      putStrLn $ "NeuralWani time: " ++ T.unpack (formatTimeNominal neuralTime)
      let speedup = realToFrac normalTime / realToFrac neuralTime :: Double
      printf "Speedup: %.2fx\n" speedup
      putStrLn $ "Results match: " ++ show (normalResult == neuralResult)
      
      putStrLn ""
      putStrLn "=========================================="
      putStrLn ""
      
      -- 結果を返す
      let normalMatch = maybe False (== normalResult) expectedResult
          neuralMatch = maybe False (== neuralResult) expectedResult
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

-- | Proverを実行して結果と実行時間を返す
runProverWithTime :: QT.Prover -> DTT.Signature -> [DTT.Preterm] -> DTT.Preterm 
                  -> IO (TI.Result, NominalDiffTime)
runProverWithTime prover sig ctx t = do
  startTime <- getCurrentTime
  
  rteResult <- NLI.runRTEWithPreterms prover sig ctx t (-1)
  let label = NLI.rtePretermLabel rteResult
  let result = case label of
        JSeM.Yes -> TI.YES
        JSeM.No  -> TI.NO
        _        -> TI.UNKNOWN
  
  endTime <- getCurrentTime
  let elapsedTime = diffUTCTime endTime startTime
  return (result, elapsedTime)

-- | Proverの結果を出力する
printProverResult :: String -> TI.Result -> NominalDiffTime -> Maybe TI.Result -> IO ()
printProverResult name result time expectedResult = do
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Time:   " ++ T.unpack (formatTimeNominal time)
  case expectedResult of
    Just expected ->
      if result == expected
        then putStrLn $ "✓ " ++ name ++ " MATCH"
        else putStrLn $ "✗ " ++ name ++ " MISMATCH: got " ++ show result ++ ", expected " ++ show expected
    Nothing -> putStrLn "? Cannot compare (no expected status)"

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
writeTexReport :: ProverConfig -> [EvalResult] -> [SkippedFile] -> IO ()
writeTexReport config results skipped = do
  -- 出力ディレクトリを作成
  createDirectoryIfMissing True "evaluateResult"
  
  -- 現在時刻を取得してファイル名に使用
  now <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" now
      -- ファイル名に maxDepth と maxTime を含める
      -- 形式: report_D{depth}T{time}_{timestamp}.tex
      configStr = "D" ++ show (cfgMaxDepth config) ++ "T" ++ show (cfgMaxTime config)
      texFilename = "evaluateResult" </> ("report_" ++ configStr ++ "_" ++ timestamp ++ ".tex")
  
  -- TeXファイルを書き込み
  T.writeFile texFilename $ generateTexContent config results skipped
  
  putStrLn ""
  putStrLn $ "TeX report written to: " ++ texFilename

-- | TeXコンテンツを生成する
generateTexContent :: ProverConfig -> [EvalResult] -> [SkippedFile] -> T.Text
generateTexContent config results skipped = T.unlines
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