# zprezto
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# zplug
if [[ -e ~/.zplug/init.zsh ]]; then
  source ~/.zplug/init.zsh
  source ~/.zsh/.zplugrc.zsh
else
  curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh| zsh
  sleep 10
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
alias cdu='cd-gitroot'
alias md='mkdir'

alias g='git'
alias gl=fshow

vif () { vim $(fzf) }
ef () { emacs -nw $(fzf) }

# path
# pyenv
if [ -x "`which pyenv`" ]; then
  export PYENV_ROOT=$HOME/.pyenv
  export PATH="$PYENV_ROOT/shims:$PATH"
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi

# rbenv
if [ -x "`which rbenv`" ]; then
  export RBENV_ROOT=$HOME/.rbenv
  eval "$(rbenv init -)"
fi

# java
export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
export PATH="$JAVA_HOME:$PATH"

# scalaenv
if [ -e ~/.scalaenv ]; then
  export PATH="${HOME}/.scalaenv/bin:${PATH}"
  eval "$(scalaenv init -)"
fi

# go
if [ -x "`which go`" ]; then
  export GOROOT=/usr/local/opt/go/libexec
  export GOPATH=$HOME/.go
  export PATH=$PATH:$GOPATH/bin
fi

# manual bin
if [ -e ~/bin ]; then
  export PATH="${HOME}/bin:${PATH}"
fi

# OPAM configuration
[ -e ~/.opam ] && source ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

#
# enhancd settings
#

# `ls` after `cd` in enhancd
ENHANCD_HOOK_AFTER_CD=l

#
# custom settings
#

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# incr
# [ -f ~/.zsh/incr/incr.zsh ] &&  source ~/.zsh/incr/incr.zsh

# npm completion
[ -f ~/.zsh/completion/npm.zsh ] && source ~/.zsh/completion/npm.zsh


#
# zsh-history-substring-search settings
#

bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# docker completion
zstyle ':completion:*:*:docker:*' option-stacking yes
zstyle ':completion:*:*:docker-*:*' option-stacking yes

delete_dotfiles () {
  find $1 \
    \( -name '.DS_Store' \
    -o -name '._*' \
    -o -name '.apdisk' \
    -o -name 'Thumbs.db' \
    -o -name 'Desktop.ini' \
    \) -delete -print;
}

dtask () { date +'%Y%m%d' }

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

if [ ~/.zshrc -nt ~/.zshrc.zwc ]; then
   zcompile ~/.zshrc
fi

if [ ~/.zsh/zplugrc.zsh -nt ~/.zsh/.zplugrc.zsh.zwc ]; then
  zcompile ~/.zsh/.zplugrc.zsh
fi
