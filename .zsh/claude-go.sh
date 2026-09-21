claude-go() {
  if [ -z "${OPENCODE_API_KEY:-}" ] && [ -r "$HOME/.zshrc.local" ]; then
    . "$HOME/.zshrc.local"
  fi
  if [ -z "${OPENCODE_API_KEY:-}" ]; then
      echo "claude-go: OPENCODE_API_KEY not found" >&2
    return 1
  fi

  OPENCODE_GO_MODEL="deepseek-v4.1-flash[1m]" \
  ANTHROPIC_BASE_URL="https://opencode.ai/zen/go/" \
  ANTHROPIC_API_KEY="${OPENCODE_API_KEY}" \
  ANTHROPIC_AUTH_TOKEN="" \
  ANTHROPIC_DEFAULT_FABLE_MODEL="kimi-k3" \
  ANTHROPIC_DEFAULT_OPUS_MODEL="${OPENCODE_GO_MODEL}" \
  ANTHROPIC_DEFAULT_SONNET_MODEL="${OPENCODE_GO_MODEL}" \
  ANTHROPIC_DEFAULT_HAIKU_MODEL="${OPENCODE_GO_MODEL}" \
  CLAUDE_CODE_SUBAGENT_MODEL="${OPENCODE_GO_MODEL}" \
  claude --permission-mode auto \
    --settings "$HOME/.claude/opencode-go.settings.json" "$@"
}
