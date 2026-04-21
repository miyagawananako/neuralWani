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
import qualified Interface.Tree as I

import qualified DTS.QueryTypes as QT
import qualified DTS.DTTdeBruijn as DTT
import qualified DTS.Prover.Wani.Prove as Prove
import qualified DTS.Prover.Wani.Arrowterm as A

import TPTP.Convert (processFile)
import qualified TPTPInfo as TI

-- | 証明木から判定と規則名のペアを抽出する
-- I.Tree QT.DTTrule DTT.Judgment を I.Tree Arrowrule AJudgment に変換してから
-- (Arrowrule, AJudgment) ペアを抽出し、AJudgment を DdB.Judgment に変換する
extractJudgmentRulePairs :: I.Tree QT.DTTrule DTT.Judgment -> [(DTT.Judgment, QT.DTTrule)]
extractJudgmentRulePairs tree = 
  let aTree = A.jTreeToaTree' tree  -- I.Tree Arrowrule AJudgment に変換
  in extractArrowPairs aTree

-- | AJudgmentツリーからペアを抽出し、AJudgmentをDdB.Judgmentに変換
extractArrowPairs :: I.Tree A.Arrowrule A.AJudgment -> [(DTT.Judgment, QT.DTTrule)]
extractArrowPairs aTree =
  let currentPair = (A.a2dtJudgment (I.node aTree), I.ruleName aTree)
      childPairs = concatMap extractArrowPairs (I.daughters aTree)
  in currentPair : childPairs

tptpDir :: FilePath
tptpDir = "data/TPTP"

targetSubDirs :: [FilePath]
targetSubDirs = ["SYN"]

defaultMaxDepth :: Int
defaultMaxDepth = 9

defaultMaxTime :: Int
defaultMaxTime = 90000

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
  let outputDir = "tptp-judgment-rule-pairs" </> ("pairs_" ++ sessionId)
  createDirectoryIfMissing True outputDir

  -- data/TPTP/ 配下のサブディレクトリからファイルを取得
  allFofFiles <- fmap concat $ mapM getFilesFromSubDir targetSubDirs
  
  -- Normal Proverで正解ラベル（Expected）と同じラベルを予測できた問題（45問）
  -- これらは既に処理済みなので除外する
  -- 設定:
  --   maxDepth: 9
  --   maxTime: 6000
  --   logicSystem: dne (Classical)
  --   Session ID: D9T6000_dne_2025-12-15_11-28-23
  -- 結果: Normal Prover Correct 45/264 (17.0%)
  let excludeFiles =
        [ ("SYN", "SYN000+1.p") -- いてみてダメだった
        , ("SYN", "SYN000+2.p")
        , ("SYN", "SYN007+1.014.p")
        , ("SYN", "SYN036+1.p")
        , ("SYN", "SYN418+1.p")
        , ("SYN", "SYN419+1.p")
        , ("SYN", "SYN420+1.p")
        , ("SYN", "SYN421+1.p")
        , ("SYN", "SYN422+1.p")
        , ("SYN", "SYN423+1.p")
        , ("SYN", "SYN424+1.p")
        , ("SYN", "SYN425+1.p")
        , ("SYN", "SYN426+1.p")
        , ("SYN", "SYN427+1.p")
        , ("SYN", "SYN428+1.p")
        , ("SYN", "SYN429+1.p")
        , ("SYN", "SYN430+1.p")
        , ("SYN", "SYN431+1.p")
        , ("SYN", "SYN432+1.p")
        , ("SYN", "SYN433+1.p")
        , ("SYN", "SYN434+1.p")
        , ("SYN", "SYN435+1.p")
        , ("SYN", "SYN436+1.p")
        , ("SYN", "SYN437+1.p")
        , ("SYN", "SYN438+1.p")
        , ("SYN", "SYN439+1.p")
        , ("SYN", "SYN440+1.p")
        , ("SYN", "SYN441+1.p")
        , ("SYN", "SYN442+1.p")
        , ("SYN", "SYN443+1.p")
        , ("SYN", "SYN444+1.p")
        , ("SYN", "SYN445+1.p")
        , ("SYN", "SYN446+1.p")
        , ("SYN", "SYN447+1.p")
        , ("SYN", "SYN448+1.p")
        , ("SYN", "SYN449+1.p")
        , ("SYN", "SYN450+1.p")
        , ("SYN", "SYN451+1.p")
        , ("SYN", "SYN452+1.p")
        , ("SYN", "SYN453+1.p")
        , ("SYN", "SYN454+1.p")
        , ("SYN", "SYN455+1.p")
        , ("SYN", "SYN456+1.p")
        , ("SYN", "SYN457+1.p")
        , ("SYN", "SYN458+1.p")
        , ("SYN", "SYN459+1.p")
        , ("SYN", "SYN460+1.p")
        , ("SYN", "SYN461+1.p")
        , ("SYN", "SYN462+1.p")
        , ("SYN", "SYN463+1.p")
        , ("SYN", "SYN464+1.p")
        , ("SYN", "SYN465+1.p")
        , ("SYN", "SYN466+1.p")
        , ("SYN", "SYN467+1.p")
        , ("SYN", "SYN468+1.p")
        , ("SYN", "SYN469+1.p")
        , ("SYN", "SYN470+1.p")
        , ("SYN", "SYN471+1.p")
        , ("SYN", "SYN472+1.p")
        , ("SYN", "SYN473+1.p")
        , ("SYN", "SYN474+1.p")
        , ("SYN", "SYN475+1.p")
        , ("SYN", "SYN476+1.p")
        , ("SYN", "SYN477+1.p")
        , ("SYN", "SYN478+1.p")
        , ("SYN", "SYN479+1.p")
        , ("SYN", "SYN480+1.p")
        , ("SYN", "SYN481+1.p")
        , ("SYN", "SYN482+1.p")
        , ("SYN", "SYN483+1.p")
        , ("SYN", "SYN484+1.p")
        , ("SYN", "SYN485+1.p")
        , ("SYN", "SYN486+1.p")
        , ("SYN", "SYN487+1.p")
        , ("SYN", "SYN488+1.p")
        , ("SYN", "SYN489+1.p")
        , ("SYN", "SYN495+1.p")
        , ("SYN", "SYN498+1.p")
        , ("SYN", "SYN499+1.p")
        , ("SYN", "SYN500+1.p")
        , ("SYN", "SYN501+1.p")
        , ("SYN", "SYN502+1.p")
        , ("SYN", "SYN503+1.p")
        , ("SYN", "SYN504+1.p")
        , ("SYN", "SYN505+1.p")
        , ("SYN", "SYN506+1.p")
        , ("SYN", "SYN507+1.p")
        , ("SYN", "SYN508+1.p")
        , ("SYN", "SYN509+1.p")
        , ("SYN", "SYN510+1.p")
        , ("SYN", "SYN511+1.p")
        , ("SYN", "SYN512+1.p")
        , ("SYN", "SYN513+1.p")
        , ("SYN", "SYN514+1.p")
        , ("SYN", "SYN518+1.p")
        , ("SYN", "SYN519+1.p")
        , ("SYN", "SYN520+1.p")
        , ("SYN", "SYN537+1.p")
        , ("SYN", "SYN538+1.p")
        , ("SYN", "SYN539+1.p")
        , ("SYN", "SYN540+1.p")
        , ("SYN", "SYN541+1.p")
        , ("SYN", "SYN542+1.p")
        , ("SYN", "SYN543+1.p")
        , ("SYN", "SYN544+1.p")
        , ("SYN", "SYN545+1.p")
        , ("SYN", "SYN546+1.p")
        , ("SYN", "SYN547+1.p")
        , ("SYN", "SYN938+1.p")
        ]
      -- 除外リストに含まれないファイルのみを処理対象とする
      fofFiles = sort $ filter (`notElem` excludeFiles) allFofFiles
  
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

