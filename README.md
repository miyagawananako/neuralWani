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

---

### 2. Evaluation (`evaluate-exe`)

Evaluates the prover on TPTP files, comparing Normal and NeuralWani provers.

> **Note:** This feature is currently under development. The NeuralWani prover is not yet integrated.

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

- TeX report: `evaluateResult/report_<config>_<timestamp>.tex`
- Proof trees (Normal): `evaluateResult/proofTrees_<config>_<timestamp>/normal/`
- Proof trees (NeuralWani): `evaluateResult/proofTrees_<config>_<timestamp>/neural/`

---

### 3. Builder (`builder-exe`)

Builds the prioritized rules function using a trained model.

```bash
stack run builder-exe
```
