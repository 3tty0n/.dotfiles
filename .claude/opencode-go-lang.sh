#!/usr/bin/env bash
# UserPromptSubmit hook for claude-go (opencode zen).
# Valid only in claude-go sessions, since it is wired up from
# ~/.claude/opencode-go.settings.json.
#
# Applies to every model served through claude-go -- the hook lives in the
# settings file, not in a per-model branch, so swapping OPENCODE_GO_MODEL
# (deepseek, kimi, qwen, ...) or running /model keeps it active.
#
# Chinese-origin models drift into Chinese once the conversation gets long,
# so the rule is re-injected on every prompt instead of being stated once at
# session start.
set -uo pipefail

POLICY=$(cat <<'EOF'
【言語ポリシー / Language policy — applies to every reply】
- 応答は日本語と英語のどちらでも、混ぜてもよい（Japanese, English, or a mix — all fine）。
- 中国語は一切使わない。Never output Chinese (Simplified or Traditional), anywhere:
  prose, headings, summaries, parenthetical glosses, translations, code comments.
- 中国語の語句が混ざりそうになったら、日本語か英語に置き換えてから出力する。
- コード・コマンド・識別子・引用したエラーメッセージの原文はそのままでよい。
- ユーザーが中国語で話しかけてきた場合も、応答は日本語か英語で返す。
EOF
)

jq -nc --arg c "$POLICY" \
  '{hookSpecificOutput: {hookEventName: "UserPromptSubmit", additionalContext: $c}}'
