# Load junegunn/fzf completion and dotfiles extensions for ghq + z.

_dotfiles_fzf_dir() {
  local -a candidates
  candidates=(
    "${FZF_DIR:-}"
    "${HOME}/.zsh/plugins/fzf"
    "${HOME}/.local/share/zinit/plugins/fzf"
    "${HOME}/.local/share/zinit/plugins/junegunn---fzf"
  )
  local d
  for d in ${candidates[@]}; do
    [[ -f "${d}/shell/completion.zsh" ]] && print -r -- "$d" && return 0
  done
  return 1
}

_dotfiles_fzf_defaults() {
  if (( $+functions[__fzf_defaults] )); then
    __fzf_defaults "$@"
  else
    print -r -- "--height ${FZF_TMUX_HEIGHT:-40%} --reverse ${2:-}"
  fi
}

_dotfiles_fzf_comprun() {
  if (( $+functions[__fzf_comprun] )); then
    __fzf_comprun "$@"
  else
    fzf "$@"
  fi
}

_dotfiles_fzf_load_completion() {
  [[ -n ${DOTFILES_FZF_COMPLETION_LOADED:-} ]] && return 0
  local fzf_dir
  fzf_dir="$(_dotfiles_fzf_dir)" || return 1
  # shellcheck disable=SC1090
  source "${fzf_dir}/shell/completion.zsh"
  typeset -g DOTFILES_FZF_COMPLETION_LOADED=1
}

_dotfiles_fzf_completion_wrap() {
  if _dotfiles_fzf_should_ghq_tab; then
    _dotfiles_ghq_fzf_tab && return
  fi
  if _dotfiles_z_fzf_tab; then
    return
  fi
  if (( $+widgets[fzf-completion-orig] )); then
    zle fzf-completion-orig
  elif (( $+widgets[expand-or-complete] )); then
    zle expand-or-complete
  fi
}

_dotfiles_fzf_bind_tab() {
  [[ ${DOTFILES_FZF_TAB_DONE:-} == 1 ]] && return 0

  if (( $+widgets[fzf-completion] )); then
    if [[ ${widgets[fzf-completion]} != user:*_dotfiles_fzf_completion_wrap ]]; then
      zle -A fzf-completion fzf-completion-orig 2>/dev/null
      zle -N fzf-completion _dotfiles_fzf_completion_wrap
    fi
    bindkey '^I' fzf-completion
  else
    zle -N _dotfiles_fzf_tab _dotfiles_fzf_completion_wrap
    bindkey '^I' _dotfiles_fzf_tab
  fi

  typeset -g DOTFILES_FZF_TAB_DONE=1
}

_dotfiles_fzf_extensions_init() {
  [[ -o interactive ]] || return 0

  local extdir="${DOT_ZSH_ROOT:-${HOME}/.zsh}/fzf-extensions"
  source "${extdir}/ghq-preview.zsh"
  source "${extdir}/ghq.zsh"
  source "${extdir}/z.zsh"

  _dotfiles_fzf_load_completion 2>/dev/null

  if [[ -o zle ]]; then
    zle -N _dotfiles_ghq_fzf_cd
    bindkey '^g' _dotfiles_ghq_fzf_cd
    _dotfiles_fzf_bind_tab
  fi

  autoload -Uz add-zsh-hook
  _dotfiles_fzf_bind_tab_precmd() {
    [[ ${DOTFILES_FZF_TAB_DONE:-} == 1 ]] && return
    _dotfiles_fzf_bind_tab
  }
  add-zsh-hook precmd _dotfiles_fzf_bind_tab_precmd
}

_dotfiles_fzf_extensions_init
