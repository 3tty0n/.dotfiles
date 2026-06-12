# FZF completion extension for ghq (GitHub repository filtering).
# Usage: ghq [get] **<TAB>  or  ghq get <TAB>  → cd into selected repo
#        Ctrl+G  → pick a repo and cd

: "${FZF_GHQ_GITHUB_ONLY:=1}"

_dotfiles_fzf_ghq_preview_cmd() {
  if [[ -n ${FZF_GHQ_PREVIEW_CMD:-} ]]; then
    print -r -- "$FZF_GHQ_PREVIEW_CMD"
    return 0
  fi
  local script="${${(%):-%x}:A:h}/fzf-ghq-preview.sh"
  if [[ -x $script ]]; then
    print -r -- "$script {}"
    return 0
  fi
  print -r -- 'ls -la "$(ghq root)/{}"'
}

_fzf_ghq_list() {
  if (( FZF_GHQ_GITHUB_ONLY )); then
    ghq list 2>/dev/null | awk '/^github\.com\//'
  else
    ghq list 2>/dev/null
  fi
}

_fzf_complete_ghq() {
  _fzf_complete \
    --reverse --tac \
    --prompt='ghq> ' \
    --header='GitHub repositories (ghq list)' \
    --preview="$(_dotfiles_fzf_ghq_preview_cmd)" \
    --bind='focus:transform-header:ghq {}/{}' \
    -- "$@" < <(_fzf_ghq_list)
}

_fzf_complete_ghq_post() {
  cat
}

_dotfiles_fzf_should_ghq_tab() {
  local -a tokens
  tokens=(${(z)LBUFFER})
  [[ ${tokens[1]} == ghq ]] || return 1
  case ${tokens[2]:-} in
    get|list) return 0 ;;
    '') (( ${#tokens} <= 2 )) && return 0 ;;
  esac
  return 1
}

_dotfiles_ghq_fzf_pick() {
  local query="${1:-}"
  local -a fzf_args
  local repo

  fzf_args=(
    --prompt='ghq> '
    --header='GitHub repositories (ghq list)'
    --preview="$(_dotfiles_fzf_ghq_preview_cmd)"
    --bind='focus:transform-header:ghq {}/{}'
  )
  [[ -n "$query" ]] && fzf_args+=(-q "$query")

  repo=$(
    FZF_DEFAULT_OPTS=$(_dotfiles_fzf_defaults '--reverse --tac' "${FZF_COMPLETION_OPTS-}") \
    FZF_DEFAULT_OPTS_FILE='' \
      _dotfiles_fzf_comprun ghq "${fzf_args[@]}" < <(_fzf_ghq_list)
  ) || return 1

  repo="${repo//$'\n'/}"
  repo="${repo#"${repo%%[![:space:]]*}"}"
  repo="${repo%"${repo##*[![:space:]]}"}"
  [[ -n "$repo" ]] || return 1
  print -r -- "$repo"
}

_dotfiles_ghq_fzf_cd() {
  local query="${1:-}"
  local repo root

  repo=$(_dotfiles_ghq_fzf_pick "$query") || {
    zle reset-prompt
    return 0
  }

  root="$(ghq root 2>/dev/null)" || {
    zle reset-prompt
    return 0
  }

  BUFFER="cd ${(q)root}/${(q)repo}"
  zle accept-line
}

_dotfiles_ghq_fzf_tab() {
  local -a tokens
  local query

  tokens=(${(z)LBUFFER})
  case ${tokens[2]:-} in
    get|list) query="${tokens[3]:-}" ;;
    *) query="${tokens[2]:-}" ;;
  esac

  _dotfiles_ghq_fzf_cd "$query"
  return 0
}
