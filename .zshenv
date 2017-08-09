typeset -gx -U path
fpath=(~/.zsh/completion \
         ~/.zsh/external/cd-gitroot(N-/) \
         ${fpath})

# autoload
autoload -Uz colors && colors
autoload -Uz cd-gitroot
#autoload -Uz compinit && compinit -C

# LANGUAGE must be set by en_US
export LANGUAGE="en_US.UTF-8"
export LANG="${LANGUAGE}"
export LC_ALL="${LANGUAGE}"
export LC_CTYPE="${LANGUAGE}"

# less
export LESS='-i -M -R -W -x4'

# ls command colors
export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

# Add ~/bin to PATH
export PATH=~/bin:"$PATH"

# pyenv
export PYENV_ROOT=$HOME/.pyenv
export PATH="$PYENV_ROOT/shims:$PATH"

# rbenv
export RBENV_ROOT=$HOME/.rbenv

# scalaenv
export PATH="${HOME}/.scalaenv/bin:${PATH}"

# java
export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
export PATH="$JAVA_HOME:$PATH"

# go
export GOROOT=/usr/local/opt/go/libexec
export GOPATH=$HOME/.go
export PATH=$PATH:$GOPATH/bin

# Add diff-highligh to PATH
export PATH=$PATH:/usr/local/share/git-core/contrib/diff-highlight

# fzf
export FZF_DEFAULT_OPTS='--height 40% --reverse --border'

# enhancd settings
ENHANCD_HOOK_AFTER_CD=l
ENHANCD_FILTER=fzf:fzy:peco

# zplug
export ZPLUG_ROOT=${HOME}/.zplug

# local settings
export DOT_ZSH_ROOT=${HOME}/.zsh

# history
HISTFILE="${HOME}/.zhistory"
