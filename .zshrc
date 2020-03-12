# {{{ Zplugin: setup
if [ ! -d $HOME/.zinit ]; then
  sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma/zplugin/master/doc/install.sh)"
fi

### Added by Zplugin's installer
source $HOME/.zinit/bin/zplugin.zsh
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit
### End of Zplugin's installer chunk

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

zinit ice lucid from"gh-r" wait"!0" as"program" bpick"${binary}" mv"*${ostype}*/bin/hub -> ${ZPFX}/bin/hub"
zinit light github/hub

zinit ice lucid from"gh-r" as"program" bpick"${binary}"
zinit load junegunn/fzf-bin

zinit ice lucid from"gh-r" as"program" bpick"${binary}" mv"*${ostype}*/ghq -> ${ZPFX}/bin/ghq"
zinit light x-motemen/ghq

zinit ice lucid from"gh-r" as"program" bpick"${binary}" mv"*${ostype}*/gist -> ${ZPFX}/bin/gist"
zinit light b4b4r07/gist

zinit ice wait"!0"; zinit light changyuheng/fz

zinit ice atclone"make" as"program" pick"fzy"; zinit load jhawthorn/fzy

zinit light zsh-users/zsh-autosuggestions

zinit light zdharma/fast-syntax-highlighting

zinit light zsh-users/zsh-history-substring-search

zinit light zsh-users/zsh-completions

zinit light hlissner/zsh-autopair

zinit load rupa/z

#zinit ice src"auto-notify.plugin.zsh"; zinit light MichaelAquilina/zsh-auto-notify

zinit ice as"program" pick"$ZPFX/bin/git-*" make"PREFIX=$ZPFX"; zinit light tj/git-extras

#zinit ice depth=1 pick"spaceship.zsh"; zinit light denysdovhan/spaceship-prompt
#zinit ice depth=1 pick"async.zsh" src"pure.zsh"; zinit light sindresorhus/pure
#zinit ice depth=1 pick"powerlevel9k.zsh-theme"; zinit light bhilburn/powerlevel9k
zinit ice depth=1; zinit light romkatv/powerlevel10k

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
alias l='ls -1a'
alias e='emacsclient -nw -a ""'
alias ekill='emacsclient -e "(kill-emacs)"'
alias emacsnw='emacs -nw'
alias g='git'
alias t='tig'
alias ta='tig --all'
alias be='bundle exec'
alias ob='ocamlbuild -use-ocamlfind'
alias luajitlatex='luajittex --fmt=luajitlatex.fmt'
alias en='emacs -nw'
alias kb='kubectl'

# use 'hightlihgt' in place of 'cat'
[[ -x "`which highlight`" ]] && \
  alias catc="highlight $1 --out-format xterm256 --line-numbers --quiet --force --style solarized-dark"

case "${OSTYPE}" in
  darwin* )
    alias ls="ls -G"
    alias ll="ls -lG"
    alias la="ls -laG"
    alias subl='/Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl'
  ;;
  linux* )
    alias ls='ls --color'
    alias ll='ls -l --color'
    alias la='ls -la --color'
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
