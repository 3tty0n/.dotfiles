__ghq() {
  local selected_dir=$(ghq list | fzy)

  if [ -n "$selected_dir" ]; then
    BUFFER="cd $(ghq root)/${selected_dir}"
    zle accept-line
  fi

  zle reset-prompt
}

zle -N __ghq
bindkey "^g" __ghq
