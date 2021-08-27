# {{{
# Change shell behavior when opening the terminal view in dolphin. MYPROMPT set by konsole profile
if ! [[ $MYPROMPT = dolphin ]]; then
    isdolphin=false
    # Use chpwd_recent_dirs to start new sessions from last working dir
    # Populate dirstack with chpwd history
    autoload -Uz chpwd_recent_dirs add-zsh-hook
    add-zsh-hook chpwd chpwd_recent_dirs
    zstyle ':chpwd:*' recent-dirs-file "${TMPDIR:-/tmp}/chpwd-recent-dirs"
    dirstack=($(awk -F"'" '{print $2}' ${$(zstyle -L ':chpwd:*' recent-dirs-file)[4]} 2>/dev/null))
    [[ ${PWD} = ${HOME}  || ${PWD} = "." ]] && (){
        local dir
        for dir ($dirstack){
            [[ -d "${dir}" ]] && { cd -q "${dir}"; break }
        }
    } 2>/dev/null
else
    isdolphin=true
fi

# Enable Powerlevel10k instant prompt
if [[ -r "${XDG_CACHE_HOME:-${HOME}/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-${HOME}/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

ZINIT_HOME="${ZINIT_HOME:-${ZPLG_HOME:-${ZDOTDIR:-${HOME}}/.zinit}}"
ZINIT_BIN_DIR_NAME="${${ZINIT_BIN_DIR_NAME:-${ZPLG_BIN_DIR_NAME}}:-bin}"
### Added by Zinit's installer
if [[ ! -f "${ZINIT_HOME}/${ZINIT_BIN_DIR_NAME}/zinit.zsh" ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing DHARMA Initiative Plugin Manager (zdharma/zinit)…%f"
    command mkdir -p "${ZINIT_HOME}" && command chmod g-rwX "${ZINIT_HOME}"
    command git clone https://github.com/zdharma/zinit "${ZINIT_HOME}/${ZINIT_BIN_DIR_NAME}" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f" || \
        print -P "%F{160}▓▒░ The clone has failed.%f"
fi
source "${ZINIT_HOME}/${ZINIT_BIN_DIR_NAME}/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit
# }}}

# {{{ Zplugin: plugin configurations
# keybind
bindkey -e
architect=""
case `uname -m` in
  i386|i686) architect="386";;
  x86_64) architect="amd";;
  arm64) architect="arm64";;
esac
ostype=""
case $OSTYPE in
  darwin*) ostype="darwin";;
  linux*) ostype="linux";;
esac
binary="*${ostype}*${architect}*"

zinit load zdharma/history-search-multi-word

# Directory listings for zsh with git features
zinit light supercrabtree/k

zinit ice atclone"make" as"program" pick"fzy"
zinit light jhawthorn/fzy

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

if [[ -x "$(command -v starship)" ]]; then
    eval "$(starship init zsh)"
else
    zinit ice depth=1; zinit light romkatv/powerlevel10k
    # To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
    [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
fi

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
alias h='hg'

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

# {{{

# OPAM
if command -v opam 1>/dev/null 2>&1; then
  if [[ -r "${HOME}"/.opam/opam-init/init.zsh ]]; then
    . "${HOME}"/.opam/opam-init/init.zsh 2> /dev/null
  fi
fi

# Rbenv
if command -v rbenv 1>/dev/null 2>&1; then
    export PATH="$HOME/.rbenv/bin:$PATH"
    eval "$(rbenv init -)"
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
