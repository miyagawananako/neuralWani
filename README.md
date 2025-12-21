# NeuralWani

NeuralWani is a system that assists proof search in Dependent Type Theory (DTT) using neural networks.

## Installation

1. Clone this repository

2. Install Stack

   https://docs.haskellstack.org/en/stable/install_and_upgrade/#manual-download

3. Install hasktorch

   https://github.com/hasktorch/hasktorch

   After installation, update the hasktorch path in `stack.yaml` to match your environment.

## Usage

### Starting Docker Container

Start the container:

```bash
docker compose up -d
```

All commands below should be executed inside the Docker container. Enter the container with:

```bash
docker compose exec hasktorch /bin/bash -c "cd /home/ubuntu/neuralWani && <command>"
```

Replace `<command>` with the stack commands described in each section below.

---

### 1. Training (`train-exe`)

Trains the neural network model.

```bash
stack run train-exe <bi> <emb> <h> <l> <bias> <lr> <steps> <iter> <delimiterToken>
```

#### Parameters

| Parameter        | Type           | Description                                | Example  |
| ---------------- | -------------- | ------------------------------------------ | -------- |
| `bi`             | Bool           | Use bidirectional LSTM (`True` or `False`) | `False`  |
| `emb`            | Int            | Embedding dimension                        | `256`    |
| `h`              | Int            | Hidden layer size                          | `256`    |
| `l`              | Int            | Number of LSTM layers                      | `1`      |
| `bias`           | Bool           | Use bias (`True` or `False`)               | `False`  |
| `lr`             | Float          | Learning rate                              | `5e-4`   |
| `steps`          | Int            | Batch size (number of steps)               | `32`     |
| `iter`           | Int            | Number of epochs                           | `10`     |
| `delimiterToken` | DelimiterToken | Type of delimiter token                    | `Unused` |

#### DelimiterToken Types

| Type     | Description                               |
| -------- | ----------------------------------------- |
| `Paren`  | Wraps tokens with parentheses `(` and `)` |
| `Sep`    | Appends a separator token at the end      |
| `Eo`     | Appends an end-of token at the end        |
| `Unused` | Uses an unused token                      |

#### Example

```bash
stack run train-exe False 256 256 1 False 5e-4 32 10 Unused
```

#### Output

Trained models are saved to:

```
trainedDataBackwardWithoutF/type<delimiterToken>_bi<bi>_s<steps>_lr<lr>_i<emb>_h<h>_layer<l>/<timestamp>/
├── seq-class.model      # Model parameters
├── frequentWords.bin    # Frequent words for tokenization
├── graph-seq-class.png  # Learning curve
├── confusion-matrix.png # Confusion matrix
├── classification-report.txt
└── training-time.txt
```

---

### 2. Extract Judgment-Rule Pairs (`extract-exe`)

Extracts judgment-rule pairs from TPTP proof trees for model evaluation.

```bash
stack run extract-exe [maxTime] [maxDepth] [logicSystem]
```

#### Parameters

| Parameter     | Type   | Description                            | Default |
| ------------- | ------ | -------------------------------------- | ------- |
| `maxTime`     | Int    | Timeout in milliseconds                | `6000`  |
| `maxDepth`    | Int    | Maximum proof search depth             | `9`     |
| `logicSystem` | String | Logic system: `plain`, `efq`, or `dne` | `dne`   |

#### Examples

```bash
# Use default settings
stack run extract-exe

# Set custom timeout
stack run extract-exe 10000

# Specify all parameters
stack run extract-exe 10000 12 dne
```

#### Output

Extracted data is saved to:

```
extractedData/pairs_D<maxDepth>T<maxTime>_<logicSystem>_<timestamp>/
├── <problem_name>.bin   # Binary format (Judgment, DTTrule) pairs
└── <problem_name>.txt   # Human-readable format (for debugging)
```

---

### 3. Evaluate Extracted Data (`eval-extract-exe`)

Evaluates a trained model on extracted judgment-rule pairs.

```bash
stack run eval-extract-exe <sessionId|directory> [modelPath] [frequentWordsPath]
```

#### Parameters

| Parameter           | Type   | Description                                        | Default                    |
| ------------------- | ------ | -------------------------------------------------- | -------------------------- |
| `sessionId`         | String | Session ID from extract-exe or full directory path | Required                   |
| `modelPath`         | String | Path to trained model                              | Default model path         |
| `frequentWordsPath` | String | Path to frequentWords.bin                          | Default frequentWords path |

#### Session ID Formats

The following formats are accepted:

- Session ID only: `D9T6000_dne_2025-12-21_12-00-00`
- With prefix: `pairs_D9T6000_dne_2025-12-21_12-00-00`
- Full path: `extractedData/pairs_D9T6000_dne_2025-12-21_12-00-00`

#### Examples

```bash
# List available extracted data directories
stack run eval-extract-exe

# Evaluate with default model
stack run eval-extract-exe D9T6000_dne_2025-12-21_12-00-00

# Specify custom model
stack run eval-extract-exe D9T6000_dne_2025-12-21_12-00-00 path/to/model.model path/to/frequentWords.bin
```

#### Output

Evaluation results are saved to:

```
evaluationResults/eval_<sessionId>_<timestamp>/
├── classification-report.txt
└── confusion-matrix.png
```

---

### 4. TPTP Evaluation (`evaluate-exe`)

Evaluates the prover on TPTP files, comparing Normal and NeuralWani provers.

> **Note:** This feature is currently under development. The NeuralWani prover is not yet fully integrated.

```bash
stack run evaluate-exe [maxTime] [maxDepth] [logicSystem]
```

#### Parameters

| Parameter     | Type   | Description                            | Default |
| ------------- | ------ | -------------------------------------- | ------- |
| `maxTime`     | Int    | Timeout in milliseconds                | `6000`  |
| `maxDepth`    | Int    | Maximum proof search depth             | `9`     |
| `logicSystem` | String | Logic system: `plain`, `efq`, or `dne` | `dne`   |

- `plain`: Uses only plain inference rules
- `efq`: Uses intuitionistic logic (ex falso quodlibet)
- `dne`: Uses classical logic (double negation elimination)

#### Examples

```bash
# Use default settings (maxTime=6000, maxDepth=9, logicSystem=dne)
stack run evaluate-exe

# Set custom timeout (10 seconds)
stack run evaluate-exe 10000

# Set timeout and depth
stack run evaluate-exe 10000 12

# Specify all parameters
stack run evaluate-exe 10000 12 efq
```

#### Output

Results are saved to:

```
evaluateResult/
├── report_<config>_<timestamp>.tex           # TeX report
├── proofTrees_<config>_<timestamp>/
│   ├── normal/                               # Proof trees (Normal prover)
│   │   ├── positive/<problem>.txt
│   │   ├── positive/<problem>.html
│   │   ├── negative/<problem>.txt
│   │   └── negative/<problem>.html
│   └── neural/                               # Proof trees (NeuralWani prover)
│       └── ...
└── perFile_<config>_<timestamp>/             # Per-file comparison reports
    └── <problem>_comparison.tex
```

---

### 5. Builder (`builder-exe`)

Builds and tests the prioritized rules function using a trained model.

```bash
stack run builder-exe
```

---

## Workflow Example

A typical workflow for training and evaluating a model:

```bash
# 1. Train a model on JSeM data
stack run train-exe False 256 256 1 False 5e-4 32 10 Unused

# 2. Extract judgment-rule pairs from TPTP problems
stack run extract-exe 6000 9 dne

# 3. Evaluate the model on extracted data
stack run eval-extract-exe D9T6000_dne_2025-12-21_12-00-00

# 4. (Optional) Run full TPTP evaluation comparing Normal vs NeuralWani
stack run evaluate-exe 6000 9 dne
```
