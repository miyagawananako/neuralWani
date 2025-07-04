# Neural Wani Makefile

.PHONY: help build test clean worktree editor

# デフォルトターゲット
help:
	@echo "Neural Wani - 利用可能なコマンド:"
	@echo ""
	@echo "  build     - プロジェクトをビルド"
	@echo "  test      - テストを実行"
	@echo "  clean     - ビルド成果物をクリーンアップ"
	@echo "  worktree  - Git worktreeを作成・管理"
	@echo "  editor    - エディタを開く"
	@echo "  help      - このヘルプを表示"

# プロジェクトビルド
build:
	stack build

# テスト実行
test:
	stack test

# クリーンアップ
clean:
	stack clean

# Worktree管理
worktree:
	@echo $(MAKECMDGOALS)
	@WORKTREE="$(filter-out $@,$(MAKECMDGOALS))"; \
	./tool/script/worktree.sh $$WORKTREE OPEN_EDITOR=$$OPEN_EDITOR

# エディタを開く
editor:
	@if [ -n "$(filter-out $@,$(MAKECMDGOALS))" ]; then \
		WT_PATH="$(filter-out $@,$(MAKECMDGOALS))"; \
		if [ -d "$$WT_PATH" ]; then \
			echo "📂 Opening editor for: $$WT_PATH"; \
			code "$$WT_PATH"; \
		else \
			echo "❌ Directory not found: $$WT_PATH"; \
			exit 1; \
		fi; \
	else \
		echo "📂 Opening editor for current directory"; \
		code .; \
	fi

# 未知のターゲットを無視
%:
	@: 