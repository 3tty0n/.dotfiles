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

export EDITOR='emacs -nw'

# less
export LESS='-gj10 --no-init --quit-if-one-screen --RAW-CONTROL-CHARS'
export LESSSOPEN='| /usr/local/bin/src-hilite-lesspipe.sh %s'

# grep
# export GREP_OPTIONS='--color=always'
# export GREP_COLOR='1;35;40'

# ls command colors
export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

# fzf
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border --inline-info'

# Add ~/usr/local/bin to PATH
export PATH=~/.local/bin:$PATH

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

# zplug
export ZPLUG_HOME=${HOME}/.zplug

# local settings
export DOT_ZSH_ROOT=${HOME}/.zsh

# OCaml
export OCAMLRUNPARAM=b
export OCAMLPARAM="_,bin-annot=1"
export OPAMKEEPBUILDDIR=1

# openssl
export PATH="/usr/local/opt/openssl/bin:$PATH"

# zplug
export PATH="$ZPLUG_HOME/bin:$PATH"

# local
export PATH="$HOME/.local/bin:$PATH"

# emacs cask
export PATH="$HOME/.cask/bin:$PATH"

# graal
export PATH="$HOME/share/graalvm-ce-1.0.0-rc8/Contents/Home/bin:$PATH"
export JAVA_HOME="$HOME/share/graalvm-ce-1.0.0-rc8/Contents/Home"
