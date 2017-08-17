#!/bin/zsh

unalias z
z() {
  if [[ -z "$*" ]]; then
    chdir "$(_z -l 2>&1 | fzf +s --tac | sed 's/^[0-9,.]* *//')"
  else
    _last_z_args="$@"
    _z "$@"
  fi
}

zz() {
  chdir "$(_z -l 2>&1 | sed 's/^[0-9,.]* *//' | fzf -q "$_last_z_args")"
}

alias j=z
alias jj=zz

__ghq-fzf() {
  local selected_dir=$(ghq list | fzf --reverse --query="$LBUFFER")

  if [ -n "$selected_dir" ]; then
    BUFFER="cd $(ghq root)/${selected_dir}"
    zle accept-line
  fi

  zle reset-prompt
}

zle -N __ghq-fzf
bindkey "^g" __ghq-fzf

__fshow () {
  { git log --graph --color=always \
      --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
  fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
      --bind "ctrl-m:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                {}
FZF-EOF" } || zle reset-prompt
}

zle -N __fshow
bindkey '^[' __fshow

__select-history() {
  local selected_cmd
  selected_cmd=$(history -n -r 1 | fzf --no-sort +m --query "$LBUFFER")
  
  if [ -n "$selected_cmd" ]; then
    BUFFER=$selected_cmd
  fi
  
  zle reset-prompt
}
zle -N __select-history
bindkey '^r' __select-history
