if [[ ! -d ~/.zplug ]]; then
    git clone https://github.com/zplug/zplug.git ~/.zplug
    source ~/.zplug/init.zsh && zplug update
fi

source ~/.zplug/init.zsh
source ~/.zsh/zplug.zsh

# internal settings
setopt auto_menu
setopt auto_cd
setopt auto_list
setopt auto_param_keys
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
alias cdu='cd-gitroot'
alias md='mkdir'
alias e='emacsclient -nw -a ""'
alias ekill='emacsclient -e "(kill-emacs)"'
alias g='git'
alias t='tig'
alias ta='tig --all'
alias l='ls -1a'
alias be='bundle exec'
alias ob='ocamlbuild -use-ocamlfind'
alias luajitlatex='luajittex --fmt=luajitlatex.fmt'
case "${OSTYPE}" in
  darwin* ) alias subl='/Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl';;
esac
alias en='emacs -nw'
alias k='kubectl'

# less
alias less='less -m -N -g -i -J --line-numbers --underline-special'
alias more='less'

# use 'hightlihgt' in place of 'cat'
alias catc="highlight $1 --out-format xterm256 --line-numbers --quiet --force --style solarized-dark"

case "${OSTYPE}" in
  darwin* )
    alias ls="ls -G"
    alias ll="ls -lG"
    alias la="ls -laG"
    # java
    export JAVA_HOME=$(/usr/libexec/java_home -v 1.8)
    export PATH="$JAVA_HOME:$PATH"
  ;;
  linux* )
    alias ls='ls --color'
    alias ll='ls -l --color'
    alias la='ls -la --color'
  ;;
esac

# OPAM
test -e "${HOME}/.opam" && \
    source ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# shell integration
test -e "${HOME}/.iterm2_shell_integration.zsh" && \
    source "${HOME}/.iterm2_shell_integration.zsh"

# load local zshrc
test -f ~/.zshrc.local && source ~/.zshrc.local

# fzf
test -f ~/.fzf.zsh && source ~/.fzf.zsh

if zplug check "jonmosco/kube-ps1"; then
    source <(kubectl completion zsh)
    PROMPT='$(kube_ps1) '$PROMPT
    KUBE_PS1_BINARY=oc
fi
