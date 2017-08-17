# zgen
source ~/.zsh/zgenrc.zsh

# internal settings
setopt auto_menu
setopt auto_cd
setopt auto_list
setopt auto_param_keys
setopt auto_param_slash
setopt list_packed
setopt rec_exact
setopt correct
setopt correct_all
setopt globdots
setopt interactive_comments
setopt no_beep
setopt no_list_beep
setopt nolistbeep
setopt no_tify
setopt list_types

alias vi='vim'
alias dc=cd
alias rm='rm -ri'
alias l='ls -1a'
alias ls='ls -G'
alias cdu='cd-gitroot'
alias md='mkdir'
alias e='emacsclient -nw -a ""'
alias ekill='emacsclient -e "(kill-emacs)"'
alias g='git'

# pyenv
if [ -x "`which pyenv`" ]; then
  eval "$(pyenv init - --no-rehash)"
  eval "$(pyenv virtualenv-init - --no-rehash)"
fi

# rbenv
if [ -x "`which rbenv`" ]; then
  eval "$(rbenv init - --no-rehash)"
fi

# scalaenv
if [ -x "`which scalaenv`" ]; then
  eval "$(scalaenv init - --no-rehash)"
fi

# hub
if [ -x "`which hub`" ]; then
  eval "$(hub alias -s)"
fi

# OPAM
if [ -e ~/.opam ]; then
  source ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
fi

# shell integration
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

ZSH_DOTFILES=(.zshrc .zshenv .zpreztorc .zsh/zplugrc.zsh)

for dotfile in ${ZSH_DOTFILES[@]}; do
  if [[ "${dotfile}" -nt "${dotfile}.zwc" ]]; then
    zcompile ${dotfile}
  fi
done
