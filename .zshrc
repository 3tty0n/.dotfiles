# zplug
if [[ -f ~/.zplug/init.zsh ]]; then
  source ${ZPLUG_ROOT}/init.zsh && source ${DOT_ZSH_ROOT}/zplugrc.zsh
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
if [ -e ~/.opam ]; then
  source ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
fi

# shell integration
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# fzf
test -f "$HOME/.fzf.zsh" && source "$HOME/.fzf.zsh"

ZSH_DOTFILES=(.zshrc .zshenv .zpreztorc .zsh/zplugrc.zsh)

for dotfile in ${ZSH_DOTFILES[@]}; do
  if [[ "{$HOME}/${dotfile}" -nt "${HOME}/${dotfile}.zwc" ]]; then
    zcompile "${HOME}/${dotfile}"
  fi
done

# load local zshrc
[ -f ~/.zshrc.local ] && source ~/.zshrc.local
