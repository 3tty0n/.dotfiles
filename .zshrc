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

source $ZSH/oh-my-zsh.sh

# path
export PYENV_ROOT="/usr/local/var/pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
eval "$(rbenv init -)"
