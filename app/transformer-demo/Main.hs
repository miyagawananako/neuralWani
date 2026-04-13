{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Control.Monad (forM_)
import Data.Maybe (fromJust)
import qualified Torch.Tensor as D
import Torch.Typed

-- Small compile-time configuration for a toy language-model task.
type NumLayers = 2
type NumHeads = 2
type FFNDim = 32
type PaddingIdx = 0
type VocabSize = 16
type EmbedDim = 16
type SeqLen = 6
type BatchSize = 4
type ModelDType = 'Float
type ModelDevice = '( 'CPU, 0)

type Model = TransformerLM NumLayers NumHeads FFNDim PaddingIdx VocabSize EmbedDim ModelDType ModelDevice
type Spec = TransformerLMSpec NumLayers NumHeads FFNDim PaddingIdx VocabSize EmbedDim ModelDType ModelDevice

numIters :: Int
numIters = 80

learningRate :: LearningRate ModelDevice ModelDType
learningRate = 5e-2

spec :: Spec
spec =
  TransformerLMSpec @NumLayers @NumHeads @FFNDim @PaddingIdx @VocabSize @EmbedDim @ModelDType @ModelDevice
    (DropoutSpec 0.1)
    ( TransformerLayerSpec
        (MultiheadAttentionSpec (DropoutSpec 0.1))
        (DropoutSpec 0.1)
        1e-5
        (TransformerMLPSpec (DropoutSpec 0.1) (DropoutSpec 0.1) 1e-5)
    )

trainX :: Tensor ModelDevice 'Int64 '[BatchSize, SeqLen]
trainX =
  ( fromJust
      [ [1, 2, 3, 4, 5, 6],
        [2, 3, 4, 5, 6, 7],
        [3, 4, 5, 6, 7, 8],
        [4, 5, 6, 7, 8, 9]
      ] :: Tensor ModelDevice 'Int64 '[BatchSize, SeqLen]
  )

validX :: Tensor ModelDevice 'Int64 '[BatchSize, SeqLen]
validX =
  ( fromJust
      [ [1, 2, 3, 4, 5, 6],
        [5, 6, 7, 8, 9, 10],
        [2, 4, 6, 8, 10, 12],
        [3, 5, 7, 9, 11, 13]
      ] :: Tensor ModelDevice 'Int64 '[BatchSize, SeqLen]
  )

lmLoss :: Model -> Tensor ModelDevice 'Int64 '[BatchSize, SeqLen] -> Tensor ModelDevice ModelDType '[]
lmLoss model tokens =
  let logits = forward model tokens
      logitsByClass = transpose @1 @2 logits
      logProbs = logSoftmax @1 logitsByClass
      classWeight = ones @'[VocabSize] @ModelDType @ModelDevice
   in nllLoss @'ReduceMean @BatchSize @VocabSize @'[SeqLen] classWeight (-100) logProbs tokens

trainLoop initModel initOptim =
  foldLoop (initModel, initOptim) numIters $ \(model, optim) stepIdx -> do
    let loss = lmLoss model trainX
        lossValue = D.asValue (toDynamic loss) :: Float
    whenEvery 10 stepIdx $ putStrLn $ "step " ++ show stepIdx ++ " trainLoss=" ++ show lossValue
    runStep model optim loss learningRate

foldLoop :: (a, b) -> Int -> ((a, b) -> Int -> IO (a, b)) -> IO (a, b)
foldLoop acc n f = go 1 acc
  where
    go i x
      | i > n = pure x
      | otherwise = f x i >>= go (i + 1)

whenEvery :: Int -> Int -> IO () -> IO ()
whenEvery k i action = if i `mod` k == 0 then action else pure ()

main :: IO ()
main = do
  putStrLn "Transformer demo: tiny LM training with hasktorch typed API"
  initModel <- sample spec
  let initOptim = mkAdam 0 0.9 0.999 (flattenParameters initModel)

  putStrLn $ "initial trainLoss=" ++ show (D.asValue (toDynamic (lmLoss initModel trainX)) :: Float)
  putStrLn $ "initial validLoss=" ++ show (D.asValue (toDynamic (lmLoss initModel validX)) :: Float)

  (trainedModel, _trainedOptim) <- trainLoop initModel initOptim

  let finalTrain = D.asValue (toDynamic (lmLoss trainedModel trainX)) :: Float
      finalValid = D.asValue (toDynamic (lmLoss trainedModel validX)) :: Float

  putStrLn $ "final trainLoss=" ++ show finalTrain
  putStrLn $ "final validLoss=" ++ show finalValid

  let logits = forward trainedModel validX
      predicted = argmax @2 @'DropDim logits
      predTokens = D.asValue (toDynamic predicted) :: [[Int]]

  putStrLn "prediction on valid batch (argmax over vocab):"
  forM_ predTokens print
