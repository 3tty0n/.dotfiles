# zprezto
[[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]] && source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"

# local settings
ZSH_CONFS=(
  external/enhancd/init.sh
  external/zsh-interactive-cd/zsh-interactive-cd.plugin.zsh
  powerline/powerline.zsh
  functions/fzf-functions.zsh
)

for conf in ${ZSH_CONFS[@]}; do
  if [[ "${conf##*.*}" = ".zsh"  && "${conf}.zsh" -nt "${conf##*/}.zwc" ]]; then
    zcompile ${conf}
  fi
  source ${DOT_ZSH_ROOT}/${conf}
done

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
alias ls='ls -G'
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
if [[ "${HOME}/.iterm2_shell_integration.zsh" ]]; then
  source "${HOME}/.iterm2_shell_integration.zsh"
fi

ZSH_DOTFILES=(.zshrc .zshenv .zpreztorc)

for dotfile in ${ZSH_DOTFILES[@]}; do
  if [[ "${dotfile##*.*}" = ".zsh"  && "${dotfile}.zsh" -nt "${dotfile##*/}.zwc" ]]; then
    zcompile ${dotfile}
  fi
done
