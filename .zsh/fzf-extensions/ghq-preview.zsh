# Expose _fzf_ghq_preview to fzf's preview subshell.
#
# zsh functions are not visible in fzf's $SHELL -c preview process, so the
# preview logic lives in ghq-preview and is invoked as an executable script.

0=${(%):-%N}
typeset -g FZF_GHQ_PREVIEW_SCRIPT="${0:A:h}/ghq-preview"

[[ -x "$FZF_GHQ_PREVIEW_SCRIPT" ]] || chmod +x "$FZF_GHQ_PREVIEW_SCRIPT" 2>/dev/null

_fzf_ghq_preview() {
  "$FZF_GHQ_PREVIEW_SCRIPT" "$@"
}

# --preview value safe for fzf (no reliance on shell functions in subshell).
_fzf_ghq_preview_opt() {
  print -r -- "${(q)FZF_GHQ_PREVIEW_SCRIPT} {}"
}

# Let fzf completion / comprun pick up ghq previews automatically.
_fzf_comprun() {
  local cmd=$1
  shift
  case $cmd in
    ghq) fzf --preview "$(_fzf_ghq_preview_opt)" "$@" ;;
    *)   fzf "$@" ;;
  esac
}
