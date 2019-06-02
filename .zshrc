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
zplugin load zdharma/history-search-multi-word

case $OSTYPE in
  darwin*)
    zplugin ice lucid from"gh-r" wait"!0" as"program" bpick"*darwin*amd*" mv"*darwin*/bin/hub -> ${ZPFX}/bin/hub"
    zplugin light github/hub

    zplugin ice from"gh-r" as"program" bpick"*darwin*amd"
    zplugin load junegunn/fzf-bin

    zplugin ice lucid from"gh-r" wait"!0" as"program" bpick"*darwin*amd*" mv"*darwin*/ghq -> ${ZPFX}/bin/ghq"
    zplugin light motemen/ghq

    zplugin ice wait"!1"; zplugin light changyuheng/fz
    ;;
  linux*)
      zplugin ice lucid from"gh-r" as"program" bpick"*linux*"
      zplugin light junegunn/fzf-bin

esac

zplugin ice atclone"make" as"program" pick"fzy"; zplugin load jhawthorn/fzy

zplugin light zsh-users/zsh-autosuggestions

zplugin light zdharma/fast-syntax-highlighting

zplugin light zsh-users/zsh-history-substring-search

zplugin light zsh-users/zsh-completions

zplugin light hlissner/zsh-autopair

zplugin ice src"z.sh"; zplugin light rupa/z

zplugin ice as"program" pick"$ZPFX/bin/git-*" make"PREFIX=$ZPFX"; zplugin light tj/git-extras

zplugin ice pick"spaceship.zsh"
zplugin light denysdovhan/spaceship-prompt

zplugin creinstall -q $HOME/.zsh/completion

zplugin ice src"util.zsh"; zplugin light $HOME/.zsh/util
# }}}

# {{{ Terminal settings for emacs
if [ "$EMACS" ]; then
  export TERM=xterm-256color
fi
# }}}

# {{{ Options
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
# }}}

#  {{{ Aliases
alias dc=cd
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
# }}}

# {{{  OPAM
test -r "${HOME}"/.opam/opam-init/init.zsh && \
  . "${HOME}"/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
# }}}

# {{{ Shell integration
if [[ $EMACS = t ]]; then
  test -e "${HOME}/.iterm2_shell_integration.zsh" && \
    source "${HOME}/.iterm2_shell_integration.zsh"
fi
# }}}

# {{{ Load local zshrc
test -f ~/.zshrc.local && source ~/.zshrc.local
# }}}
