# zprezto
[[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]] && source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"

#
# custom settings
#

# zplug
if [[ -e ~/.zplug/init.zsh ]]; then
  source ~/.zplug/init.zsh
  source ~/.zsh/zplugrc.zsh
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
alias dc=cd
alias rm='rm -ri'
alias l='ls -1a'
alias cdu='cd-gitroot'
alias md='mkdir'
alias e='emacsclient -nw -a ""'
alias emacs='emacsclient -nw -a ""'
alias ekill='emacsclient -e "(kill-emacs)"'
alias g='git'
alias gl=fshow

vif () { vim $(fzf) }
ef () { emacs -nw $(fzf) }
dtask () { date +'%Y%m%d' }

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
if [ -x "`which java`" ]; then
  export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
  export PATH="$JAVA_HOME:$PATH"
fi

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

# powerline
# [ -f ~/.local/lib/python2.7/site-packages/powerline/bindings/zsh/powerline.zsh ] && source ~/.local/lib/python2.7/site-packages/powerline/bindings/zsh/powerline.zsh

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

#
# enhancd settings
#

# `ls` after `cd` in enhancd
ENHANCD_HOOK_AFTER_CD=l

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

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

if [ ~/.zshrc -nt ~/.zshrc.zwc ]; then
   zcompile ~/.zshrc
fi

if [ ~/.zsh/zplugrc.zsh -nt ~/.zsh/zplugrc.zsh.zwc ]; then
  zcompile ~/.zsh/zplugrc.zsh
fi
