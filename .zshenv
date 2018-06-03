typeset -gx -U path
fpath=(${HOME}/.zsh/completion ${fpath})

# autoload
autoload -Uz colors && colors
autoload -Uz cd-gitroot

# LANGUAGE must be set by en_US
export LANGUAGE="en_US.UTF-8"
export LANG="${LANGUAGE}"
export LC_ALL="${LANGUAGE}"
export LC_CTYPE="${LANGUAGE}"

export TERM="xterm-256color"

# less
export LESS='-gj10 --no-init --quit-if-one-screen --RAW-CONTROL-CHARS'
export LESSSOPEN='| /usr/local/bin/src-hilite-lesspipe.sh %s'

# grep
# export GREP_OPTIONS='--color=always'
# export GREP_COLOR='1;35;40'

# ls command colors
export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

# Add ~/usr/local/bin to PATH
# export PATH=~/.local/bin:$PATH

# pyenv
export PYENV_ROOT=$HOME/.pyenv
export PATH="$PYENV_ROOT/bin:$PATH"

# rbenv
export RBENV_ROOT=$HOME/.rbenv
export PATH="$RBENV_ROOT/bin:$PATH"

# scalaenv
export PATH="${HOME}/.scalaenv/bin:${PATH}"

# go
export GOPATH=$HOME/.go
export PATH=$PATH:$GOPATH/bin

# Add diff-highligh to PATH
export PATH=$PATH:/usr/local/share/git-core/contrib/diff-highlight

# fzf
export FZF_DEFAULT_OPTS="--height 40% --reverse --border --color fg:242,bg:236,hl:65,fg+:15,bg+:239,hl+:108"

# enhancd settings
ENHANCD_HOOK_AFTER_CD='ls -1a'
ENHANCD_FILTER=fzf:fzy:peco

# zplug
export ZPLUG_HOME=${HOME}/.zplug

# local settings
export DOT_ZSH_ROOT=${HOME}/.zsh

# OCaml
export OCAMLRUNPARAM=b
export OCAMLPARAM="_,bin-annot=1"
export OPAMKEEPBUILDDIR=1

# java
export JAVA_HOME=$(/usr/libexec/java_home -v 1.8)
export PATH="$JAVA_HOME:$PATH"

# openssl
export PATH="/usr/local/opt/openssl/bin:$PATH"

# zplug
export PATH="$ZPLUG_HOME/bin:$PATH"

# local
export PATH="$HOME/.local/bin:$PATH"

# emacs cask
export PATH="$HOME/.cask/bin:$PATH"

# powerlevel9k setting
POWERLEVEL9K_SHORTEN_DIR_LENGTH=1
POWERLEVEL9K_SHORTEN_DELIMITER=""
POWERLEVEL9K_SHORTEN_STRATEGY="truncate_from_right"

# prompt
POWERLEVEL9K_PROMPT_ON_NEWLINE=true
POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX=""
POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX="▷ "

POWERLEVEL9K_CUSTOM_WIFI_SIGNAL="zsh_wifi_signal"
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(status context dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(history time)
