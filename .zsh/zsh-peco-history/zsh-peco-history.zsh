__peco-select-history () {
  local tac
  if which tac > /dev/null; then
    tac="tac"
  else
    tac="tail -r"
  fi
  BUFFER=$(\history -n 1 | \
             eval $tac | \
             peco --query "$LBUFFER")
  CURSOR=$#BUFFER
}
zle -N __peco-select-history
bindkey '^r' __peco-select-history
