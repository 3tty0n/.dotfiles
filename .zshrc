#!/bin/bash

if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# internal settings
setopt auto_menu
setopt auto_cd
setopt auto_list
setopt auto_param_keys
setopt auto_param_slash
setopt auto_param_slash
setopt list_packed
setopt rec_exact
setopt correct
setopt globdots
setopt interactive_comments
setopt no_beep
setopt nolistbeep
setopt no_tify
setopt list_types

alias vi='vim'
alias e='emacs -nw'
alias dc=cd
alias rm='rm -ri'
alias scala='scala -Dscala.color'
alias cdu='cd-gitroot'

function vimf () { vim $(fzf) }
function ef () { emacs -nw $(fzf) }

# path
# pyenv
export PYENV_ROOT=$HOME/.pyenv
export PATH="$PYENV_ROOT/shims:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# rbenv
export RBENV_ROOT=$HOME/.rbenv
eval "$(rbenv init -)"

# java
export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
export PATH="$JAVA_HOME:$PATH"

# scalaenv
export PATH="${HOME}/.scalaenv/bin:${PATH}"
eval "$(scalaenv init -)"

# manual bin
export PATH="${HOME}/bin:${PATH}"

# OPAM configuration
source ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# incr
[ -f ~/.zsh/incr/incr.zsh ] &&  source ~/.zsh/incr/incr.zsh

# npm completion
[ -f ~/.zsh/completion/npm.zsh ] && source ~/.zsh/completion/npm.zsh

# fshow - git commit browser
function fshow() {
  git log --graph --color=always \
      --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
  fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
      --bind "ctrl-m:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                {}
FZF-EOF"
}

function delete_dotfiles () {
  find $1 \( -name '.DS_Store' -o -name '._*' -o -name '.apdisk' -o -name 'Thumbs.db' -o -name 'Desktop.ini' \) -delete -print;
}

function dtask () {
  date +'%Y%m%d'
}

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# zplug
if [[ -e ~/.zplug/init.zsh ]]; then
  source ~/.zplug/init.zsh
  source ~/.dotfiles/.zplugrc.zsh
else
  curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh| zsh
  sleep 10
fi
