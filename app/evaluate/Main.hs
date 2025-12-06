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
import Data.List (isInfixOf, sort)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Exception (try, SomeException)
import Text.Read (readMaybe)

-- | 評価結果を保持するデータ型
data EvalResult = EvalResult
  { erFilename       :: String
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
  { sfFilename :: String
  , sfReason   :: String
  } deriving (Show)

-- | data/SYN/ ディレクトリのパス
synDir :: FilePath
synDir = "data/SYN"

-- | Proverの設定パラメータ（デフォルト値）
defaultMaxDepth :: Int
defaultMaxDepth = 5
-- defaultMaxDepth = 9

defaultMaxTime :: Int
defaultMaxTime = 10
-- defaultMaxTime = 10000

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
  let singleFile = "SYN000+1.p"
  putStrLn $ "Processing single file: " ++ singleFile
  result <- processOneFile config singleFile
  case result of
    Left skipped -> putStrLn $ "Skipped: " ++ sfReason skipped
    Right eval   -> writeTexReport config [eval] []
  
  -- ========================================
  -- 全FOFファイルを処理する場合はこちら（デフォルト）
  -- ========================================
  -- -- data/SYN/ ディレクトリ内のファイルを取得
  -- allFiles <- listDirectory synDir
  
  -- -- ファイル名に "+" が含まれているものだけをフィルタリング（FOF形式）
  -- let fofFiles = sort $ filter ('+' `elem`) allFiles
  
  -- -- Found 376 FOF files (with '+' in filename)！！
  -- putStrLn $ "Found " ++ show (length fofFiles) ++ " FOF files (with '+' in filename)"
  -- putStrLn ""
  
  -- -- 各ファイルを処理して結果を集計
  -- results <- mapM (processOneFile config) fofFiles
  
  -- -- 成功した結果とスキップされたファイルを分離
  -- let (skipped, evaluated) = partitionResults results
  
  -- -- TeX形式でレポートを出力
  -- writeTexReport config evaluated skipped
  
  -- -- サマリーを表示
  -- printSummary evaluated skipped

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
  
  info <- processFile filepath
  
  -- TPTPファイルから取得したデータを変数に格納
  let context   :: [DTT.Preterm]
      context   = TI.context info
      
      target    :: Maybe DTT.Preterm
      target    = TI.target info
      
      signature :: DTT.Signature
      signature = TI.signature info
  
  putStrLn $ "=== File: " ++ TI.filename info ++ " ==="
  putStrLn $ "Status: " ++ show (TI.status info)
  putStrLn $ "Language: " ++ show (TI.language info)
  putStrLn ""
  
  putStrLn "=== Axioms (context) ==="
  mapM_ print (reverse context)
  putStrLn ""
  
  putStrLn "=== Conjecture (target) ==="
  case target of
    Just t  -> print t
    Nothing -> putStrLn "No conjecture found"
  putStrLn ""
  
  putStrLn "=== Signature ==="
  mapM_ print signature
  
  if null (TI.note info)
    then return ()
    else putStrLn $ "\nNote: " ++ TI.note info
  
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
  
  case target of
    Just t -> do
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
      putStrLn $ "Normal time:     " ++ formatTimeNominal normalTime
      putStrLn $ "NeuralWani time: " ++ formatTimeNominal neuralTime
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
        { erFilename     = filename
        , erExpected     = expectedResult
        , erNormalResult = normalResult
        , erNormalTime   = normalTime
        , erNeuralResult = neuralResult
        , erNeuralTime   = neuralTime
        , erNormalMatch  = normalMatch
        , erNeuralMatch  = neuralMatch
        }
      
    Nothing -> do
      putStrLn "target is Nothing"
      putStrLn ""
      putStrLn "=========================================="
      putStrLn ""
      return $ Left SkippedFile
        { sfFilename = filename
        , sfReason   = "No conjecture found"
        }

-- | Proverを実行して結果と実行時間を返す
runProverWithTime :: QT.Prover -> DTT.Signature -> [DTT.Preterm] -> DTT.Preterm 
                  -> IO (TI.Result, NominalDiffTime)
runProverWithTime prover sig ctx t = do
  startTime <- getCurrentTime
  
  -- 1. まず conjecture を証明しようとする
  rteResult <- NLI.runRTEWithPreterms prover sig ctx t (-1)
  let posLabel = NLI.rtePretermLabel rteResult
  
  -- 2. conjecture が証明できなかったら ¬conjecture を試す
  result <- case posLabel of
    JSeM.Yes -> return TI.YES  -- conjecture が証明できた → YES
    _ -> do
      -- ¬conjecture を証明しようとする
      let negTarget = DTT.Not t
      negResult <- NLI.runRTEWithPreterms prover sig ctx negTarget (-1)
      let negLabel = NLI.rtePretermLabel negResult
      case negLabel of
        JSeM.Yes -> return TI.NO  -- ¬conjecture が証明できた → NO
        _        -> return TI.UNKNOWN  -- どちらも証明できなかった → UNKNOWN
  
  endTime <- getCurrentTime
  let elapsedTime = diffUTCTime endTime startTime
  return (result, elapsedTime)

-- | Proverの結果を出力する
printProverResult :: String -> TI.Result -> NominalDiffTime -> Maybe TI.Result -> IO ()
printProverResult name result time expectedResult = do
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Time:   " ++ formatTimeNominal time
  case expectedResult of
    Just expected ->
      if result == expected
        then putStrLn $ "✓ " ++ name ++ " MATCH"
        else putStrLn $ "✗ " ++ name ++ " MISMATCH: got " ++ show result ++ ", expected " ++ show expected
    Nothing -> putStrLn "? Cannot compare (no expected status)"

-- | 時間をフォーマットする
formatTimeNominal :: NominalDiffTime -> String
formatTimeNominal t = printf "%.3f sec" (realToFrac t :: Double)

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
  putStrLn $ "Normal:     " ++ formatTimeNominal avgNormalTime
  putStrLn $ "NeuralWani: " ++ formatTimeNominal avgNeuralTime

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
  writeFile texFilename $ generateTexContent config results skipped
  
  putStrLn ""
  putStrLn $ "TeX report written to: " ++ texFilename

-- | TeXコンテンツを生成する
generateTexContent :: ProverConfig -> [EvalResult] -> [SkippedFile] -> String
generateTexContent config results skipped = unlines
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
  , "\\item maxDepth: " ++ show (cfgMaxDepth config)
  , "\\item maxTime: " ++ show (cfgMaxTime config)
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
generateSummaryTex :: [EvalResult] -> [SkippedFile] -> String
generateSummaryTex results skipped = unlines
  [ "\\begin{tabular}{ll}"
  , "\\toprule"
  , "\\textbf{Metric} & \\textbf{Value} \\\\"
  , "\\midrule"
  , "Total files processed & " ++ show (length results + length skipped) ++ " \\\\"
  , "Evaluated & " ++ show (length results) ++ " \\\\"
  , "Skipped & " ++ show (length skipped) ++ " \\\\"
  , "\\midrule"
  , "Normal Prover Correct & " ++ show normalMatches ++ "/" ++ show totalEval 
      ++ " (" ++ printf "%.1f" normalAcc ++ "\\%) \\\\"
  , "NeuralWani Prover Correct & " ++ show neuralMatches ++ "/" ++ show totalEval 
      ++ " (" ++ printf "%.1f" neuralAcc ++ "\\%) \\\\"
  , "\\midrule"
  , "Avg. Normal Time & " ++ formatTimeNominal avgNormalTime ++ " \\\\"
  , "Avg. NeuralWani Time & " ++ formatTimeNominal avgNeuralTime ++ " \\\\"
  , "\\bottomrule"
  , "\\end{tabular}"
  ]
  where
    normalMatches = length $ filter erNormalMatch results
    neuralMatches = length $ filter erNeuralMatch results
    totalEval = length results
    normalAcc = if totalEval == 0 then 0 else 100.0 * fromIntegral normalMatches / fromIntegral totalEval :: Double
    neuralAcc = if totalEval == 0 then 0 else 100.0 * fromIntegral neuralMatches / fromIntegral totalEval :: Double
    avgNormalTime = if totalEval == 0 then 0 else sum (map erNormalTime results) / fromIntegral totalEval
    avgNeuralTime = if totalEval == 0 then 0 else sum (map erNeuralTime results) / fromIntegral totalEval

-- | 結果テーブルのTeXを生成
generateResultsTable :: [EvalResult] -> String
generateResultsTable results = unlines $
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
resultToTexRow :: EvalResult -> String
resultToTexRow r = concat
  [ escapeTeX (erFilename r), " & "
  , maybe "-" show (erExpected r), " & "
  , show (erNormalResult r), " & "
  , formatTimeShort (erNormalTime r), " & "
  , matchSymbol (erNormalMatch r), " & "
  , show (erNeuralResult r), " & "
  , formatTimeShort (erNeuralTime r), " & "
  , matchSymbol (erNeuralMatch r), " & "
  , printf "%.2fx" speedup
  , " \\\\"
  ]
  where
    speedup = realToFrac (erNormalTime r) / realToFrac (erNeuralTime r) :: Double
    matchSymbol True  = "\\cellcolor{matchcolor}$\\checkmark$"
    matchSymbol False = "\\cellcolor{mismatchcolor}$\\times$"

-- | 短い時間フォーマット
formatTimeShort :: NominalDiffTime -> String
formatTimeShort t = printf "%.3fs" (realToFrac t :: Double)

-- | スキップされたファイルのセクションを生成
generateSkippedSection :: [SkippedFile] -> String
generateSkippedSection skipped = unlines $
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
skippedToTexRow :: SkippedFile -> String
skippedToTexRow s = escapeTeX (sfFilename s) ++ " & " ++ escapeTeX (sfReason s) ++ " \\\\"

-- | TeXの特殊文字をエスケープ
escapeTeX :: String -> String
escapeTeX = concatMap escapeChar
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
    escapeChar c   = [c]