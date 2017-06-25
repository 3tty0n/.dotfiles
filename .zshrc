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

function vimf() {  vim $(fzf) }
function ef() { emacs -nw $(fzf) }

# external settings
# source $HOME/.dotfiles/sshagent.sh

# path
# pyenv
export PYENV_ROOT=$HOME/.pyenv
export PATH="$PYENV_ROOT/shims:$PATH"
eval "$(pyenv init -)"

# rbenv
export RBENV_ROOT=$HOME/.rbenv
eval "$(pyenv virtualenv-init -)"
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

fpath=(/usr/local/share/zsh-completions $fpath)

function delete_dotfiles () {
  find $1 \( -name '.DS_Store' -o -name '._*' -o -name '.apdisk' -o -name 'Thumbs.db' -o -name 'Desktop.ini' \) -delete -print;
}

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# zplug
if [[ -e ~/.zplug/init.zsh ]]; then
  source ~/.zplug/init.zsh
else
  curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh| zsh
  sleep 10
fi

zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-syntax-highlighting", defer:2
zplug "zsh-users/zsh-history-substring-search"
zplug "mollifier/cd-gitroot"
zplug "b4b4r07/enhancd", use:enhancd.sh
zplug "junegunn/fzf-bin", as:command, from:gh-r, rename-to:fzf
zplug "peco/peco", as:command, from:gh-r, use:"*amd64*"

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

# Then, source plugins and add commands to $PATH
zplug load --verbose
