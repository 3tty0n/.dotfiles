### Added by Zinit's installer
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

bindkey -e
architect=""
case `uname -m` in
  i386|i686) architect="386";;
  arm64) architect="arm64";;
  x86_64) architect="amd";;
  arm64) architect="arm64";;
esac
ostype=""
case $OSTYPE in
  darwin*) ostype="darwin";;
  linux*) ostype="linux";;
esac
binary="*${ostype}*${architect}*"

zinit load zdharma-continuum/history-search-multi-word

# Directory listings for zsh with git features
zinit light supercrabtree/k

zinit ice atclone"make" as"program" pick"fzy"
zinit light jhawthorn/fzy

zinit light changyuheng/fz

zinit light rupa/z

zinit light zsh-users/zsh-autosuggestions

zinit light zdharma-continuum/fast-syntax-highlighting

zinit light zsh-users/zsh-history-substring-search

zinit ice blockf atpull'zinit creinstall -q .'
zinit light zsh-users/zsh-completions

zinit light hlissner/zsh-autopair

zinit ice src"auto-notify.plugin.zsh"; zinit light MichaelAquilina/zsh-auto-notify

zinit ice as"program" pick"$ZPFX/bin/git-*" make"PREFIX=$ZPFX"; zinit light tj/git-extras

if [[ -x "$(command -v starship)" ]]; then
    eval "$(starship init zsh)"
else
    zinit ice depth=1; zinit light romkatv/powerlevel10k
    # To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
    [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
fi

zinit creinstall -q $HOME/.zsh/completion

zinit ice src"util.zsh"; zinit light $HOME/.zsh/util

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

alias dc=cd
alias md='mkdir'
alias rm='rm -ri'
alias g='git'
alias h='hg'

alias vi='vim'

if [ -x "$(command -v emacs)" ]; then
    alias e='emacsclient'
fi

if [ -x "$(command -v tig)" ]; then
    alias t='tig'
    alias ta='tig --all'
fi

if [ -x "$(command -v verco)" ] && [ -x "$(command -v hg)" ]; then
    alias gh='verco'
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

PYPY3HACK=~/src/foss.heptapod.net/pypy/pypy-3.7-hack/pypy/goal/pypy3-c
[ -f $PYPY3HACK ] && alias pypy3-hack=$PYPY3HACK

PYPY37=~/src/foss.heptapod.net/pypy/pypy-3.7/pypy/gloal/pypy3-c
[ -f $PYPY37 ] && alias pypy37=$PYPY3

alias jitviewer=~/src/github.com/prg-titech/jitviewer/bin/jitviewer.py

# OPAM
if command -v opam 1>/dev/null 2>&1; then
  if [[ -r "${HOME}"/.opam/opam-init/init.zsh ]]; then
    . "${HOME}"/.opam/opam-init/init.zsh 2> /dev/null
  fi
fi

# Rbenv
if [ -d ~/.rbenv ]; then
    export PATH="$HOME/.rbenv/bin:$PATH"
    eval "$(rbenv init -)"
fi

if [ -f ~/.zshrc.local ]; then
    source ~/.zshrc.local
fi

if [ -f ~/.profile ]; then
    source ~/.profile
fi
# }}}

# {{{ Shell integration
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
# }}}

# {{{ vterm for emacs
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
# }}}

# {{{ Load local configuration files
test -f ~/.zshrc.local && source ~/.zshrc.local
# }}}
