if [[ ! -d ~/.zplug ]]; then
    git clone https://github.com/zplug/zplug.git ~/.zplug
    source ~/.zplug/init.zsh && zplug update
fi

source ~/.zplug/init.zsh
source ~/.zsh/zplug.zsh
# source ~/.dotfiles/scripts/tmux.sh

# terminal settings for emacs
if [ "$EMACS" ]; then
  export TERM=xterm-256color
  # unsetopt zle
fi

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
setopt share_history
setopt list_packed

alias dc=cd
alias cdu='cd-gitroot'
alias md='mkdir'
alias rm='rm -ri'
alias l='ls -1a'
alias e='emacsclient -nw -a ""'
alias ekill='emacsclient -e "(kill-emacs)"'
alias g='git'
alias t='tig'
alias ta='tig --all'
alias be='bundle exec'
alias ob='ocamlbuild -use-ocamlfind'
alias luajitlatex='luajittex --fmt=luajitlatex.fmt'
case "${OSTYPE}" in
  darwin* ) alias subl='/Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl';;
esac
alias en='emacs -nw'
alias kb='kubectl'

# use 'hightlihgt' in place of 'cat'
alias catc="highlight $1 --out-format xterm256 --line-numbers --quiet --force --style solarized-dark"

case "${OSTYPE}" in
  darwin* )
    alias ls="ls -G"
    alias ll="ls -lG"
    alias la="ls -laG"
  ;;
  linux* )
    alias ls='ls --color'
    alias ll='ls -l --color'
    alias la='ls -la --color'
  ;;
esac

# OPAM
test -r "${HOME}"/.opam/opam-init/init.zsh && \
  . "${HOME}"/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# shell integration
if [[ $EMACS = t ]]; then
  test -e "${HOME}/.iterm2_shell_integration.zsh" && \
  source "${HOME}/.iterm2_shell_integration.zsh"
fi

# rust
if [[ -f "`which rustc`" ]]; then
  source ~/.cargo/env
fi

# load local zshrc
test -f ~/.zshrc.local && source ~/.zshrc.local
