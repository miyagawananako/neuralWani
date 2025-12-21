{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Data.Text.Lazy as T      --text
import qualified Data.Text.Lazy.IO as T   --text
import qualified Data.ByteString as B     --bytestring
import Data.List (sort, intercalate)
import Data.Maybe (fromJust)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Store (encode)
import System.Environment (getArgs)
import System.Directory (listDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), takeBaseName)
import System.Mem (performMajorGC)
import Text.Read (readMaybe)
import Control.DeepSeq (rnf)
import Control.Exception (evaluate, try, SomeException)
import qualified ListT

import qualified DTS.QueryTypes as QT
import qualified DTS.DTTdeBruijn as DTT
import qualified DTS.Prover.Wani.Prove as Prove
import qualified Interface.Tree as I

import TPTP.Convert (processFile)
import qualified TPTPInfo as TI

-- | 証明木から判定と規則名のペアを抽出する
extractJudgmentRulePairs :: I.Tree QT.DTTrule DTT.Judgment -> [(DTT.Judgment, QT.DTTrule)]
extractJudgmentRulePairs tree = 
  let currentPair = (I.node tree, I.ruleName tree)
      childPairs = concatMap extractJudgmentRulePairs (I.daughters tree)
  in currentPair : childPairs

tptpDir :: FilePath
tptpDir = "data/TPTP"

targetSubDirs :: [FilePath]
targetSubDirs = ["SYN"]

defaultMaxDepth :: Int
defaultMaxDepth = 9

defaultMaxTime :: Int
defaultMaxTime = 6000

data ProverConfig = ProverConfig
  { cfgMaxDepth     :: Int
  , cfgMaxTime      :: Int
  , cfgLogicSystem  :: Maybe QT.LogicSystem  -- Nothing=plain, Intuitionistic=efq, Classical=dne
  } deriving (Show)

main :: IO()
main = do
  -- コマンドライン引数からmaxTime, maxDepth, logicSystemを取得
  args <- getArgs
  let config = case args of
        -- デフォルト: Classical（dne）を使用
        [] -> ProverConfig defaultMaxDepth defaultMaxTime (Just QT.Classical)
        [timeStr] -> case readMaybe timeStr of
          Just t  -> ProverConfig defaultMaxDepth t (Just QT.Classical)
          Nothing -> error $ "Invalid maxTime: " ++ timeStr ++ "\n" ++ usageMsg
        [timeStr, depthStr] -> case (readMaybe timeStr, readMaybe depthStr) of
          (Just t, Just d) -> ProverConfig d t (Just QT.Classical)
          (Nothing, _)     -> error $ "Invalid maxTime: " ++ timeStr ++ "\n" ++ usageMsg
          (_, Nothing)     -> error $ "Invalid maxDepth: " ++ depthStr ++ "\n" ++ usageMsg
        [timeStr, depthStr, logicStr] -> case (readMaybe timeStr, readMaybe depthStr, parseLogicSystem logicStr) of
          (Just t, Just d, Just ls) -> ProverConfig d t ls
          (Nothing, _, _)     -> error $ "Invalid maxTime: " ++ timeStr ++ "\n" ++ usageMsg
          (_, Nothing, _)     -> error $ "Invalid maxDepth: " ++ depthStr ++ "\n" ++ usageMsg
          (_, _, Nothing)     -> error $ "Invalid logicSystem: " ++ logicStr ++ " (use 'plain', 'efq', or 'dne')\n" ++ usageMsg
        _ -> error usageMsg
      usageMsg = "Usage: extract-exe [maxTime] [maxDepth] [plain|efq|dne]\n"
      -- Convert command line argument to Maybe QT.LogicSystem
      parseLogicSystem "plain" = Just Nothing
      parseLogicSystem "efq"   = Just (Just QT.Intuitionistic)
      parseLogicSystem "dne"   = Just (Just QT.Classical)
      parseLogicSystem _       = Nothing
  
  -- 実行開始時刻を取得
  now <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" now
      logicStr = logicSystemToStr (cfgLogicSystem config)
      configStr = "D" ++ show (cfgMaxDepth config) ++ "T" ++ show (cfgMaxTime config) ++ "_" ++ logicStr
      sessionId = configStr ++ "_" ++ timestamp
  
  putStrLn $ "=== Extract Judgment-Rule Pairs ==="
  putStrLn $ "maxDepth:     " ++ show (cfgMaxDepth config)
  putStrLn $ "maxTime:      " ++ show (cfgMaxTime config)
  putStrLn $ "logicSystem:  " ++ logicStr
  putStrLn $ "Session:      " ++ sessionId
  putStrLn ""

  -- 出力ディレクトリを作成
  let outputDir = "extractedData" </> ("pairs_" ++ sessionId)
  createDirectoryIfMissing True outputDir

  -- data/TPTP/ 配下のサブディレクトリからファイルを取得
  -- fofFilesWithSubDir <- fmap concat $ mapM getFilesFromSubDir targetSubDirs
  -- let fofFiles = sort fofFilesWithSubDir
  
  -- Normal Proverで正解ラベル（Expected）と同じラベルを予測できた問題（45問）
  -- 設定:
  --   maxDepth: 9
  --   maxTime: 6000
  --   logicSystem: dne (Classical)
  --   Session ID: D9T6000_dne_2025-12-15_11-28-23
  -- 結果: Normal Prover Correct 45/264 (17.0%)
  let fofFiles =
        [ ("SYN", "SYN001+1.p")
        , ("SYN", "SYN040+1.p")
        , ("SYN", "SYN041+1.p")
        , ("SYN", "SYN055+1.p")
        , ("SYN", "SYN355+1.p")
        , ("SYN", "SYN357+1.p")
        , ("SYN", "SYN378+1.p")
        , ("SYN", "SYN387+1.p")
        , ("SYN", "SYN388+1.p")
        , ("SYN", "SYN390+1.p")
        , ("SYN", "SYN394+1.p")
        , ("SYN", "SYN395+1.p")
        , ("SYN", "SYN399+1.p")
        , ("SYN", "SYN400+1.p")
        , ("SYN", "SYN401+1.p")
        , ("SYN", "SYN402+1.p")
        , ("SYN", "SYN403+1.p")
        , ("SYN", "SYN405+1.p")
        , ("SYN", "SYN406+1.p")
        , ("SYN", "SYN408+1.p")
        , ("SYN", "SYN416+1.p")
        , ("SYN", "SYN915+1.p")
        , ("SYN", "SYN916+1.p")
        , ("SYN", "SYN923+1.p")
        , ("SYN", "SYN926+1.p")
        , ("SYN", "SYN927+1.p")
        , ("SYN", "SYN929+1.p")
        , ("SYN", "SYN933+1.p")
        , ("SYN", "SYN945+1.p")
        , ("SYN", "SYN953+1.p")
        , ("SYN", "SYN955+1.p")
        , ("SYN", "SYN956+1.p")
        , ("SYN", "SYN958+1.p")
        , ("SYN", "SYN959+1.p")
        , ("SYN", "SYN961+1.p")
        , ("SYN", "SYN962+1.p")
        , ("SYN", "SYN964+1.p")
        , ("SYN", "SYN969+1.p")
        , ("SYN", "SYN970+1.p")
        , ("SYN", "SYN972+1.p")
        , ("SYN", "SYN974+1.p")
        , ("SYN", "SYN975+1.p")
        , ("SYN", "SYN976+1.p")
        , ("SYN", "SYN978+1.p")
        , ("SYN", "SYN981+1.p")
        ]
  
  putStrLn $ "Found " ++ show (length fofFiles) ++ " FOF files (with '+' in filename)"
  putStrLn ""
  
  -- 各ファイルを処理
  results <- mapM (processOneFile config outputDir) fofFiles
  
  -- 成功した結果とスキップされたファイルを分離
  let (skipped, extracted) = partitionResults results
  
  -- サマリーを表示
  printSummary outputDir extracted skipped

-- | LogicSystemを文字列に変換
logicSystemToStr :: Maybe QT.LogicSystem -> String
logicSystemToStr Nothing                  = "plain"
logicSystemToStr (Just QT.Intuitionistic) = "efq"
logicSystemToStr (Just QT.Classical)      = "dne"

-- | 結果を分離する
partitionResults :: [Either String (String, Int)] -> ([String], [(String, Int)])
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
processOneFile :: ProverConfig -> FilePath -> (FilePath, FilePath) -> IO (Either String (String, Int))
processOneFile config outputDir (subDir, filename) = do
  let filepath = tptpDir </> subDir </> filename
      filenameText = subDir </> filename
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
      putStrLn $ "=== SKIP: " ++ filenameText ++ " ==="
      putStrLn $ "  Reason: " ++ intercalate ", " errors
      return $ Left filenameText
    else do
      let t    = fromJust target
      
      putStrLn $ "=== File: " ++ filenameText ++ " ==="
      
      -- Proverの設定
      let setting = QT.defaultProofSearchSetting {
                QT.maxDepth = Just (cfgMaxDepth config),
                QT.maxTime = Just (cfgMaxTime config),
                QT.logicSystem = cfgLogicSystem config
                }
      
      -- 共通データの強制評価 (Warm-up)
      evaluate $ rnf (context, signature, t)

      -- 証明探索を実行して判定-規則ペアを抽出
      result <- try $ extractPairsFromProof setting signature context t
      case result of
        Left (e :: SomeException) -> do
          putStrLn $ "  [ERROR] Exception: " ++ show e
          return $ Left filenameText
        Right (pairs, posCount, negCount) -> do
          let totalPairs = length pairs
              baseName = takeBaseName filename
              outputFile = outputDir </> (baseName ++ ".bin")
          
          putStrLn $ "  Positive tree pairs: " ++ show posCount
          putStrLn $ "  Negative tree pairs: " ++ show negCount
          putStrLn $ "  Total pairs: " ++ show totalPairs
          
          if totalPairs > 0
            then do
              -- バイナリファイルに保存
              B.writeFile outputFile (encode pairs)
              putStrLn $ "  Saved to: " ++ outputFile
              
              -- 人間が読める形式でも出力（デバッグ用）
              let textFile = outputDir </> (baseName ++ ".txt")
              T.writeFile textFile $ formatPairs pairs
              putStrLn $ "  Text format: " ++ textFile
              
              return $ Right (filenameText, totalPairs)
            else do
              putStrLn "  No proof found, skipping..."
              return $ Left filenameText

-- | 証明探索を実行してペアを抽出
extractPairsFromProof :: QT.ProofSearchSetting -> DTT.Signature -> [DTT.Preterm] -> DTT.Preterm
                      -> IO ([(DTT.Judgment, QT.DTTrule)], Int, Int)
extractPairsFromProof setting sig ctx targetType = do
  performMajorGC
  
  let negationType = DTT.Pi targetType DTT.Bot
  
  -- positive（targetType）の証明を試みる
  (posTrees, _) <- runProveWithTree setting sig ctx targetType
  let posPairs = concatMap extractJudgmentRulePairs posTrees
  
  -- negative（¬targetType）の証明を試みる
  (negTrees, _) <- runProveWithTree setting sig ctx negationType
  let negPairs = concatMap extractJudgmentRulePairs negTrees
  
  return (posPairs ++ negPairs, length posPairs, length negPairs)

-- | prove' を使って証明探索を実行し、最初の証明木を取得する
runProveWithTree :: QT.ProofSearchSetting -> DTT.Signature -> [DTT.Preterm] -> DTT.Preterm
                 -> IO ([I.Tree QT.DTTrule DTT.Judgment], Double)
runProveWithTree setting sig ctx targetType = do
  startTime <- getCurrentTime
  
  let query = DTT.ProofSearchQuery sig ctx targetType
      prover = Prove.prove' setting
  
  -- 最初の証明木のみを取得
  maybeTree <- ListT.head (prover query)
  let trees = case maybeTree of
        Nothing -> []
        Just tree -> [tree]
  
  endTime <- getCurrentTime
  let elapsedTime = realToFrac $ diffUTCTime endTime startTime :: Double
  
  return (trees, elapsedTime)

-- | ペアを人間が読める形式にフォーマット
formatPairs :: [(DTT.Judgment, QT.DTTrule)] -> T.Text
formatPairs pairs = T.unlines $ zipWith formatPair [1..] pairs
  where
    formatPair :: Int -> (DTT.Judgment, QT.DTTrule) -> T.Text
    formatPair idx (judgment, rule) = T.concat
      [ T.pack $ show idx, ". "
      , T.pack "Rule: ", T.pack $ show rule, "\n"
      , T.pack "   Judgment: ", T.pack $ show judgment, "\n"
      ]

-- | サマリーを表示
printSummary :: FilePath -> [(String, Int)] -> [String] -> IO ()
printSummary outputDir extracted skipped = do
  putStrLn ""
  putStrLn "============================================"
  putStrLn "=== SUMMARY ==="
  putStrLn "============================================"
  putStrLn $ "Output directory: " ++ outputDir
  putStrLn $ "Total files processed: " ++ show (length extracted + length skipped)
  putStrLn $ "Successfully extracted: " ++ show (length extracted)
  putStrLn $ "Skipped: " ++ show (length skipped)
  putStrLn ""
  
  let totalPairs = sum $ map snd extracted
  putStrLn $ "Total judgment-rule pairs extracted: " ++ show totalPairs
  
  when (not $ null skipped) $ do
    putStrLn ""
    putStrLn "--- Skipped files ---"
    mapM_ putStrLn skipped
  where
    when cond action = if cond then action else return ()

