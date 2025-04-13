# NeuralWani

Neural Wani は、ニューラルネットワークを使用して Dependent Type Theory (DTT)の証明探索を補助するシステムです。

## インストール方法

1.  clone this repository

2.  install stack

    https://docs.haskellstack.org/en/stable/install_and_upgrade/#manual-download

3.  install hasktorch

    https://github.com/hasktorch/hasktorch

    インストール後に、neuralWani レポジトリの stack.yaml の hasktorch のパスを自身の環境に書き換える。

## 使用方法

#### Docker の場合

`$ docker compose up -d`でコンテナを立ち上げる。

### 学習の前処理

#### Docker の場合

1. `$ docker compose exec hasktorch /bin/bash -c "cd /home/ubuntu/neuralWani && stack run preprocess-exe`を実行して前処理を行う。

#### GPU マシンの場合

1. `$ stack run preprocess-exe`を実行して前処理を行う。

### 学習

#### Docker の場合

1. `$ docker compose exec hasktorch /bin/bash -c "cd /home/ubuntu/neuralWani && stack run train-exe <bi> <emb> <h> <l> <bias> <lr> <steps> <iter> <delimiterToken>"`を実行して学習する。

#### GPU マシンの場合

1. `$ stack run train-exe <bi> <emb> <h> <l> <bias> <lr> <steps> <iter> <delimiterToken>"`を実行して学習する。

### 学習パラメータの説明

`train-exe`コマンドは以下の引数を取ります：

1. `bi` (Bool): 双方向 LSTM を使用するかどうか

   - `False`: 単方向 LSTM
   - `True`: 双方向 LSTM

2. `emb` (Int): 埋め込み層の次元数

   - 例: `256`

3. `h` (Int): 隠れ層のサイズ

   - 例: `256`

4. `l` (Int): LSTM の層数

   - 例: `1`

5. `bias` (Bool): バイアスを使用するかどうか

   - `False`: バイアスなし
   - `True`: バイアスあり

6. `lr` (Float): 学習率

   - 例: `5e-4`

7. `steps` (Int): ステップ数

   - 例: `32`

8. `iter` (Int): エポック数

   - 例: `10`

9. `delimiterToken` (DelimiterToken): デリミタトークンの種類
   - 例: `Unused`

#### 使用例

```bash
stack run train-exe False 256 256 1 False 5e-4 32 10 Unused
```
