claude-go() {
  if [ -z "${OPENCODE_API_KEY:-}" ] && [ -r "$HOME/.zshrc.local" ]; then
    . "$HOME/.zshrc.local"
  fi
  if [ -z "${OPENCODE_API_KEY:-}" ]; then
      echo "claude-go: OPENCODE_API_KEY not found" >&2
    return 1
  fi

  ANTHROPIC_BASE_URL="https://opencode.ai/zen/go/" \
  ANTHROPIC_API_KEY="${OPENCODE_API_KEY}" \
  ANTHROPIC_AUTH_TOKEN="" \
  ANTHROPIC_DEFAULT_FABLE_MODEL="glm-5.3" \
  ANTHROPIC_DEFAULT_OPUS_MODEL="deepseek-v4.1-flash[1m]" \
  ANTHROPIC_DEFAULT_SONNET_MODEL="mimo-v2.6-flash" \
  ANTHROPIC_DEFAULT_HAIKU_MODEL="mimo-v2.6-flash" \
  CLAUDE_CODE_SUBAGENT_MODEL="deepseek-v4.1-flash[1m]" \
  claude --permission-mode auto \
    --model sonnet \
    --settings "$HOME/.claude/opencode-go.settings.json" \
    "$@"
}
