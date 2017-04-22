#!/bin/bash
export ZSH=$HOME/.oh-my-zsh
ZSH_THEME="agnoster"

#plugins
plugins=(git theme ruby osx bundler brew emoji-clock sublime)

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

alias vi=vim
alias dc=cd
alias rm='rm -rf'
alias lsa='ls -a'
alias scala='scala -Dscala.color'

# external settings
source $ZSH/oh-my-zsh.sh
source $HOME/.dotfiles/sshagent.sh
# source $HOME/.dotfiles/tmux/tmux.sh

# path
export PYENV_ROOT="/usr/local/var/pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
eval "$(rbenv init -)"

brew-cask-upgrade(){ for app in $(brew cask list); do local latest="$(brew cask info "${app}" | awk 'NR==1{print $2}')"; local versions=($(ls -1 "/usr/local/Caskroom/${app}/.metadata/")); local current=$(echo ${versions} | awk '{print $NF}'); if [[ "${latest}" = "latest" ]]; then echo "[!] ${app}: ${current} == ${latest}"; [[ "$1" = "-f" ]] && brew cask install "${app}" --force; continue; elif [[ "${current}" = "${latest}" ]]; then continue; fi; echo "[+] ${app}: ${current} -> ${latest}"; brew cask uninstall "${app}" --force; brew cask install "${app}"; done; }
