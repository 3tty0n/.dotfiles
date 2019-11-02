# {{{ Zplugin: setup
if [ ! -d $HOME/.zplugin ]; then
  sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma/zplugin/master/doc/install.sh)"
fi

### Added by Zplugin's installer
source $HOME/.zplugin/bin/zplugin.zsh
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin
### End of Zplugin's installer chunk

# }}}

# {{{ Zplugin: plugin configurations
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

zplugin load zdharma/history-search-multi-word

zplugin ice lucid from"gh-r" wait"!0" as"program" bpick"${binary}" mv"*${ostype}*/bin/hub -> ${ZPFX}/bin/hub"
zplugin light github/hub

zplugin ice lucid from"gh-r" as"program" bpick"${binary}"
zplugin load junegunn/fzf-bin

zplugin ice lucid from"gh-r" wait"!0" as"program" bpick"${binary}" mv"*${ostype}*/ghq -> ${ZPFX}/bin/ghq"
zplugin light motemen/ghq

zplugin ice lucid from"gh-r" as"program" bpick"${binary}" mv"*${ostype}*/gist -> ${ZPFX}/bin/gist"
zplugin light b4b4r07/gist

zplugin ice wait"!0"; zplugin light changyuheng/fz

zplugin ice atclone"make" as"program" pick"fzy"; zplugin load jhawthorn/fzy

zplugin light zsh-users/zsh-autosuggestions

zplugin light zdharma/fast-syntax-highlighting

zplugin light zsh-users/zsh-history-substring-search

zplugin light zsh-users/zsh-completions

zplugin light hlissner/zsh-autopair

#zplugin ice src"z.sh"; zplugin light rupa/z

#zplugin ice src"auto-notify.plugin.zsh"; zplugin light MichaelAquilina/zsh-auto-notify

zplugin ice as"program" pick"$ZPFX/bin/git-*" make"PREFIX=$ZPFX"; zplugin light tj/git-extras

#zplugin ice pick"spaceship.zsh"; zplugin light denysdovhan/spaceship-prompt
zplugin ice pick"async.zsh" src"pure.zsh"; zplugin light sindresorhus/pure

zplugin creinstall -q $HOME/.zsh/completion

zplugin ice src"util.zsh"; zplugin light $HOME/.zsh/util
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

# keybind
bindkey -e
# }}}

# {{{  Package Managers
# OPAM
test -r "${HOME}"/.opam/opam-init/init.zsh && \
  . "${HOME}"/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# Pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi
# }}}

# {{{ Shell integration
if [[ $EMACS = t ]]; then
  test -e "${HOME}/.iterm2_shell_integration.zsh" && \
    source "${HOME}/.iterm2_shell_integration.zsh"
fi
# }}}

# {{{ Load local configuration files
test -f ~/.zshrc.local && source ~/.zshrc.local
test -f ~/.profile && source ~/.profile
# }}}
