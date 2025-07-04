#!/usr/bin/env bash
set -Eeuo pipefail

###############################################################################
# Git Worktree管理スクリプト
# 
# 使用方法:
#   ./worktree.sh <worktree-name>
#   OPEN_EDITOR=false ./worktree.sh <worktree-name>
#
# 引数:
#   worktree-name: 作成するworktreeの名前
#
# 環境変数:
#   GWQ_BASE_DIR: worktreeを作成するベースディレクトリ (デフォルト: ~/worktrees)
#   OPEN_EDITOR: エディタを開くかどうか (デフォルト: true)
###############################################################################

# 設定
WORK_ROOT="${GWQ_BASE_DIR:-$HOME/worktrees/github.com/neuralWani}"
OPEN_EDITOR="${OPEN_EDITOR:-true}"

# エラーハンドリング
die() {
  echo "❌ $*" >&2
  exit 1
}

# 使用方法を表示
usage() {
  cat << EOF
Usage: $(basename "$0") <worktree-name>

Arguments:
  worktree-name   Name of the worktree to create

Environment variables:
  GWQ_BASE_DIR    Base directory for worktrees (default: ~/worktrees)
  OPEN_EDITOR     Whether to open editor (default: true)

Examples:
  $(basename "$0") issue-123
  OPEN_EDITOR=false $(basename "$0") issue-123
EOF
  exit 1
}

# 引数チェック
if [[ $# -eq 0 ]]; then
  usage
fi

WORKTREE_NAME="$1"

# Worktreeのパス（スラッシュをハイフンに置換）
WORKTREE_DIR_NAME="${WORKTREE_NAME//\//-}"
WT_PATH="$WORK_ROOT/$WORKTREE_DIR_NAME"

# ベースディレクトリを作成
mkdir -p "$WORK_ROOT"

# Worktreeの作成または既存のものを使用
if [[ -d "$WT_PATH" ]]; then
  echo "🌳 Worktree already exists at: $WT_PATH"
else
  echo "🌳 Creating worktree: $WORKTREE_NAME at $WT_PATH"
  git fetch origin main
  
  # ブランチが既に存在するかチェック
  if git show-ref --verify --quiet "refs/heads/$WORKTREE_NAME"; then
    echo "📌 Branch $WORKTREE_NAME already exists locally, using existing branch"
    git worktree add "$WT_PATH" "$WORKTREE_NAME" || die "Failed to create worktree with existing branch"
  elif git ls-remote --exit-code --heads origin "$WORKTREE_NAME" >/dev/null 2>&1; then
    echo "📌 Branch $WORKTREE_NAME exists on remote, checking out from remote"
    git worktree add -b "$WORKTREE_NAME" "$WT_PATH" "origin/$WORKTREE_NAME" || die "Failed to create worktree from remote branch"
  else
    echo "🌱 Creating new branch $WORKTREE_NAME from origin/main"
    git worktree add -b "$WORKTREE_NAME" "$WT_PATH" origin/main || die "Failed to create worktree"
  fi
fi

echo "✅ Worktree ready: $WT_PATH"

# エディタを開く
if [[ "$OPEN_EDITOR" == "true" ]]; then
  # プロジェクトルートに移動してmake editorを実行
  PROJECT_ROOT="$(git rev-parse --show-toplevel)"
  cd "$PROJECT_ROOT"
  make editor "$WT_PATH"
fi

# 最後の行にWorktreeのパスを出力（他のスクリプトから使用するため）
echo "$WT_PATH" 