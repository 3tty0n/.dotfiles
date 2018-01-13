function __fzf-select-history() {
  BUFFER=$(history -n 1 | tail -r  | awk '!a[$0]++' | fzf --no-sort +m --prompt="History > ")
  CURSOR=$#BUFFER
  zle reset-prompt
}

zle -N __fzf-select-history
bindkey '^r' __fzf-select-history
