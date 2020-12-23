# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# {{{ Zplugin: setup
if [ ! -d $HOME/.zinit ]; then
  sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma/zplugin/master/doc/install.sh)"
fi

ZINIT_HOME="${ZINIT_HOME:-${ZPLG_HOME:-${ZDOTDIR:-$HOME}/.zinit}}"
ZINIT_BIN_DIR="${${ZINIT_BIN_DIR_NAME:-$ZPLG_BIN_DIR_NAME}:-bin}"
### Added by Zinit's installer
if [[ ! -f $ZINIT_HOME/$ZINIT_BIN_DIR/zinit.zsh ]]; then
  print -P "%F{33}▓▒░ %F{220}Installing DHARMA Initiative Plugin Manager (zdharma/zinit)…%f"
  command mkdir -p $ZINIT_HOME
  command git clone https://github.com/zdharma/zinit $ZINIT_HOME/$ZINIT_BIN_DIR_NAME && \\
    print -P "%F{33}▓▒░ %F{34}Installation successful.%f" || \\
    print -P "%F{160}▓▒░ The clone has failed.%f"
fi
source "$ZINIT_HOME/$ZINIT_BIN_DIR/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit
### End of Zinit installer's chunk

# }}}

# {{{ Zplugin: plugin configurations
# keybind
bindkey -e
architect=""
case `uname -m` in
  i386|i686) architect="386";;
  x86_64) architect="amd";;
esac
ostype=""
case $OSTYPE in
  darwin*) ostype="darwin";;
  linux*) ostype="linux";;
esac
binary="*${ostype}*${architect}*"

zinit load zdharma/history-search-multi-word

# Enable `hub'
# zinit ice from"gh-r" as"program" bpick"${binary}" mv"*${ostype}*/bin/hub -> ${ZPFX}/bin/hub" atload"eval '$(hub alias -s)'"
# zinit light github/hub

# Directory listings for zsh with git features
zinit light supercrabtree/k

zinit ice from"gh-r" as"program" bpick"${binary}"
zinit load junegunn/fzf-bin

zinit ice from"gh-r" as"program" bpick"${binary}" mv"*${ostype}*/ghq -> ${ZPFX}/bin/ghq"
zinit light x-motemen/ghq

zinit ice atclone"make" as"program" pick"fzy"
zinit light jhawthorn/fzy

zinit ice from"gh-r" as"program" bpick"${binary}" mv"*${ostype}*/peco -> ${ZPFX}/bin/peco"
zinit light peco/peco

zinit light changyuheng/fz

zinit light rupa/z

zinit light zsh-users/zsh-autosuggestions

zinit light zdharma/fast-syntax-highlighting

zinit light zsh-users/zsh-history-substring-search

zinit ice blockf atpull'zinit creinstall -q .'
zinit light zsh-users/zsh-completions

zinit light hlissner/zsh-autopair

#zinit ice src"auto-notify.plugin.zsh"; zinit light MichaelAquilina/zsh-auto-notify

zinit ice as"program" pick"$ZPFX/bin/git-*" make"PREFIX=$ZPFX"; zinit light tj/git-extras

#zinit ice depth=1 pick"spaceship.zsh"; zinit light denysdovhan/spaceship-prompt
#zinit ice depth=1; zinit light romkatv/powerlevel10k

[ -x "$(command -v starship)" ] && eval "$(starship init zsh)"


zinit creinstall -q $HOME/.zsh/completion

zinit ice src"util.zsh"; zinit light $HOME/.zsh/util
# }}}

# {{{ Options
setopt auto_menu
setopt auto_cd
setopt auto_list
setopt auto_param_keys
setopt auto_param_slash
setopt auto_resume
setopt list_packed
setopt rec_exact
setopt correct
setopt complete_in_word
setopt globdots
setopt interactive_comments
setopt inc_append_history
setopt hist_no_store
setopt HIST_REDUCE_BLANKS
setopt no_beep
setopt nolistbeep
setopt notify
setopt list_types
setopt share_history
setopt list_packed
# }}}

#  {{{ Aliases
alias dc=cd
alias md='mkdir'
alias rm='rm -ri'
alias g='git'

alias vi='vim'

if [ -x "$(command -v emacs)" ]; then
  alias e='emacsclient'
fi

if [ -x "$(command -v tig)" ]; then
  alias t='tig'
  alias ta='tig --all'
fi

if [ -x "$(command -v bundle)" ]; then
  alias be='bundle exec'
fi

case "${OSTYPE}" in
  darwin* )
    alias l='ls -1a -G'
    alias ls="ls -G"
    alias ll="ls -lG"
    alias la="ls -laG"
    alias subl='/Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl'
  ;;
  linux* )
    alias l='ls -1a --color'
    alias ls='ls --color'
    alias ll='ls -l --color'
    alias la='ls -la --color'

    alias blank='sleep 0.2; xset dpms force off'
    alias mutt=neomutt
  ;;
esac

# }}}

# {{{  Package Managers
# OPAM
if command -v opam 1>/dev/null 2>&1; then
  if [[ -r "${HOME}"/.opam/opam-init/init.zsh ]]; then
    . "${HOME}"/.opam/opam-init/init.zsh 2> /dev/null
  fi
fi

# Pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

# Rbenv
export RBENV_ROOT="$HOME/.rbenv"
export PATH="$RBENV_ROOT/bin:$PATH"
if command -v rbenv 1>/dev/null 2>&1; then
  eval "$(rbenv init -)"
fi
# }}}

# {{{ Shell integration
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
# }}}

# {{{ Load local configuration files
test -f ~/.zshrc.local && source ~/.zshrc.local
test -f ~/.profile && source ~/.profile
test -f ~/.p10k.zsh && source ~/.p10k.zsh
# }}}
### End of Zinit's installer chunk

# To customize prompt, run `p10k configure` or edit ~/.dotfiles/.p10k.zsh.
[[ ! -f ~/.dotfiles/.p10k.zsh ]] || source ~/.dotfiles/.p10k.zsh
