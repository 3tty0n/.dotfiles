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


alias j='z'
alias jj='zz'


__ghq_fzf() {
  local selected_dir=$(ghq list | fzf --reverse --query="$LBUFFER")

  if [ -n "$selected_dir" ]; then
    BUFFER="cd $(ghq root)/${selected_dir}"
    zle accept-line
  fi

  zle reset-prompt
}

zle -N __ghq_fzf
bindkey "^g" __ghq_fzf


__fzf_show () {
  { git log --graph --color=always \
      --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
  fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
      --bind "ctrl-m:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                {}
FZF-EOF"
  } || zle reset-prompt
}

zle -N __fzf_show
bindkey '^[' __fzf_show


fsn () {
  local TEMPLATE="$(curl https://github.com/foundweekends/giter8/wiki/giter8-templates -s | grep "\.g8<" | sed -e "s/</ /g" -e "s/>/ /g" | awk '{print $3}' | fzf | head -n 1)"
  if [[ -z "$TEMPLATE" ]]; then
    return
  fi
  sbt new $TEMPLATE
  rm -rf target || exit 1
}

__gol_fzf () {
  local selected_key=$(gol ls | sort | fzf | sed -e "s/[a-zA-Z0-9-]*: //g")

  if [ -n "$selected_key" ]; then
    open $selected_key
  fi
  zle reset-prompt
}

zle -N __gol_fzf
bindkey '^]' __gol_fzf

fco() {
  local tags branches target
  tags=$(
    git tag | awk '{print "\x1b[31;1mtag\x1b[m\t" $1}') || return
  branches=$(
    git branch --all | grep -v HEAD             |
    sed "s/.* //"    | sed "s#remotes/[^/]*/##" |
    sort -u          | awk '{print "\x1b[34;1mbranch\x1b[m\t" $1}') || return
  target=$(
    (echo "$tags"; echo "$branches") |
    fzf-tmux -l30 -- --no-hscroll --ansi +m -d "\t" -n 2) || return
  git checkout $(echo "$target" | awk '{print $2}')
}
