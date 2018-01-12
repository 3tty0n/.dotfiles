__fzf-select-history() {
  BUFFER=$(history 1 |
      sort -k1,1nr |
      perl -ne 'BEGIN { my @lines = (); } s/^\s*\d+\*?\s*//; $in=$_; if (!(grep {$in eq $_} @lines)) { push(@lines, $in); print $in; }' |
      fzf --query "$LBUFFER")
  CURSOR=${#BUFFER}
  zle reset-prompt
}
zle -N __fzf-select-history
bindkey '^r' __fzf-select-history
