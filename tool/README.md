# Neural Wani - Worktree 管理ツール

このツールは、Neural Wani プロジェクトで複数のブランチを効率的に管理するための Git worktree 操作を提供します。

## 前提条件

- Git
- VS Code (エディタとして使用)

## インストール

特別なインストールは不要です。Git の標準的な worktree 機能を使用します。

## 使用方法

### 基本的な使用方法

```bash
# 新しいworktreeを作成
make worktree feature-branch-name

# エディタを開かずにworktreeを作成
OPEN_EDITOR=false make worktree feature-branch-name
```

### 例

```bash
# 新しい機能ブランチを作成
make worktree feature/user-authentication

# バグ修正ブランチを作成
make worktree bugfix/login-error

# 既存のリモートブランチをチェックアウト
make worktree hotfix/security-patch
```

## 環境変数

- `GWQ_BASE_DIR`: worktree を作成するベースディレクトリ (デフォルト: `~/worktrees/github.com/neuralWani`)
- `OPEN_EDITOR`: エディタを開くかどうか (デフォルト: `true`)

## 動作

1. 指定された名前の worktree が存在しない場合：

   - ローカルブランチが存在する場合は既存ブランチを使用
   - リモートブランチが存在する場合はリモートからチェックアウト
   - どちらも存在しない場合は`origin/main`から新しいブランチを作成

2. worktree が既に存在する場合：

   - 既存の worktree のパスを表示

3. エディタが有効な場合：
   - VS Code で worktree ディレクトリを開く

## ファイル構造

```
tool/
├── script/
│   └── worktree.sh    # worktree管理スクリプト
└── README.md          # このファイル
```

## トラブルシューティング

### 権限エラーが発生する場合

```bash
chmod +x tool/script/worktree.sh
```

### worktree ディレクトリが見つからない場合

`GWQ_BASE_DIR`環境変数を設定して、worktree の保存場所を指定してください：

```bash
export GWQ_BASE_DIR="$HOME/my-worktrees"
make worktree feature-branch
```

### Git worktree の一覧を確認する場合

```bash
git worktree list
```

### 不要な worktree を削除する場合

```bash
git worktree remove <worktree-path>
```
