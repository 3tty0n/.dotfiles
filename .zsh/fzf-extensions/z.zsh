# FZF completion for z (zoxide or rupa/z).
# Usage: z **<TAB>  or  z <TAB>  (when tab integration is enabled)

_fzf_z_list() {
  if (( $+commands[zoxide] )); then
    zoxide query -ls 2>/dev/null | while IFS= read -r line; do
      # "score path" → keep for display; post-processor takes the path
      print -r -- "$line"
    done
  elif (( $+functions[z] )) || (( $+commands[z] )); then
    z -l 2>&1 | while IFS= read -r line; do
      [[ "$line" == common:* ]] && continue
      print -r -- "$line"
    done
  fi
}

_fzf_z_path_from_line() {
  if (( $+commands[zoxide] )); then
    # zoxide query -ls: "<score> <path>"
    awk '{ $1=""; sub(/^ /,""); print }'
  else
    awk '{print $2}'
  fi
}

_fzf_complete_z() {
  _fzf_complete \
    --reverse --tac --nth=2.. \
    --prompt='z> ' \
    --header='Frequent directories' \
    -- "$@" < <(_fzf_z_list)
}

_fzf_complete_z_post() {
  _fzf_z_path_from_line
}

_dotfiles_z_fzf_tab() {
  local -a tokens
  local query selected dir

  tokens=(${(z)LBUFFER})
  [[ ${tokens[1]} == z ]] || return 1

  (( ${#tokens} > 1 )) && query="${tokens[2,-1]}"

  selected=$(
    FZF_DEFAULT_OPTS=$(_dotfiles_fzf_defaults '--reverse --tac --nth=2..' "${FZF_COMPLETION_OPTS-}") \
    FZF_DEFAULT_OPTS_FILE='' \
      _dotfiles_fzf_comprun z -q "${(j: :)query}" < <(_fzf_z_list)
  ) || return 0

  dir=$(print -r -- "$selected" | _fzf_z_path_from_line)

  if [[ -n "$dir" ]]; then
    LBUFFER="z ${(q)dir}"
    CURSOR=${#LBUFFER}
  fi
  zle reset-prompt
  return 0
}
