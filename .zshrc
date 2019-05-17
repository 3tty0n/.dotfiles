# {{{ Zplugin: setup
if [ ! -d $HOME/.zplugin ]; then
  sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma/zplugin/master/doc/install.sh)"
fi

### Added by Zplugin's installer
source '/Users/izawa/.zplugin/bin/zplugin.zsh'
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin
### End of Zplugin's installer chunk

# }}}

# {{{ Zplugin: plugin configurations
zplugin load zdharma/history-search-multi-word

zplugin ice from"gh-r" as"program"; zplugin load junegunn/fzf-bin

zplugin ice lucid from"gh-r" wait"!0" as"program" bpick"*darwin_amd*" mv"*darwin*/ghq -> ${ZPFX}/bin/ghq"
zplugin light motemen/ghq

zplugin ice lucid from"gh-r" wait"!0" as"program" bpick"*darwin*" mv"*darwin*/bin/hub -> ${ZPFX}/bin/hub"
zplugin light github/hub

zplugin ice atclone"make" as"program" pick"fzy"; zplugin load jhawthorn/fzy

zplugin light zsh-users/zsh-autosuggestions

zplugin light zdharma/fast-syntax-highlighting

zplugin load zsh-users/zsh-history-substring-search

zplugin load zsh-users/zsh-completions

zplugin load hlissner/zsh-autopair

zplugin ice src"z.sh"; zplugin light rupa/z

zplugin ice wait"!1"; zplugin light changyuheng/fz

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

# {{{ history-substring-search
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
# }}}

# {{{ tmux-powerline
function mute_powerline_left {
  bash ~/.tmux/tmux-powerline/mute_powerline.sh left
}

function mute_powerline_right {
  bash ~/.tmux/tmux-powerline/mute_powerline.sh right
}

zle -N mute_powerline_left
zle -N mute_powerline_right
bindkey '^[' mute_powerline_left
bindkey '^]' mute_powerline_right
# }}}

# {{{ Z + Fuzzy finder
type fzy >/dev/null 2>&1 && j() {
  local recentd
  z -l | tail -r | awk '{ print $2 }' | fzy | read recentd && cd $recentd
}
# }}}

# {{{ Load local zshrc
# emacs
function estart() {
  if ! emacsclient -e 0 > /dev/null 2>&1; then
    cd > /dev/null 2>&1
    emacs --daemon
    cd - > /dev/null 2>&1
  fi
}

# load local zshrc
test -f ~/.zshrc.local && source ~/.zshrc.local
# }}}
