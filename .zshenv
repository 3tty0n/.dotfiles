typeset -gx -U path
fpath=(${HOME}/.zsh/completion ${fpath})
autoload -Uz compinit && compinit -i

# autoload
autoload -Uz colors && colors
autoload -Uz cd-gitroot

# LANGUAGE must be set by en_US
export LANGUAGE="en_US.UTF-8"
export LANG="${LANGUAGE}"
export LC_ALL="${LANGUAGE}"
export LC_CTYPE="${LANGUAGE}"

export TERM="xterm-256color"

export EDITOR='vim'

# grep
# export GREP_OPTIONS='--color=always'
# export GREP_COLOR='1;35;40'

# ls command colors
export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

# fzf
export FZF_DEFAULT_OPTS="--height 40% --style full --preview 'fzf-preview.sh {}' --bind 'focus:transform-header:file --brief {}'"

# Add ~/usr/local/bin to PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/usr/local/bin:$PATH

# rbenv
export RBENV_ROOT=$HOME/.rbenv
export PATH="$RBENV_ROOT/bin:$PATH"
export PATH="$RBENV_ROOT/shims:$PATH"

# scalaenv
export PATH="${HOME}/.scalaenv/bin:${PATH}"

# go
export GOPATH=$HOME/.go
export PATH=$PATH:$GOPATH/bin

# local settings
export DOT_ZSH_ROOT=${HOME}/.zsh

# OCaml
export OCAMLRUNPARAM=b
export OCAMLPARAM="_,bin-annot=1"
export OPAMKEEPBUILDDIR=1

# openssl
export PATH="/usr/local/opt/openssl/bin:$PATH"

# local
export PATH="$HOME/.local/bin:$PATH"

# emacs cask
export PATH="$HOME/.cask/bin:$PATH"

# jdk
export JAVA_HOME=/usr/lib/jvm/default

# graal
export GRAALVM_HOME=$HOME/.local/share/graalvm-jdk-25.0.1-8.1
export PATH="${GRAALVM_HOME}/bin:$PATH"
export JAVA_HOME="${GRAALVM_HOME}"
#export GRAALVM_HOME="$HOME/.local/share/graalvm-community-openjdk-23.0.2"
#export PATH="${GRAALVM_HOME}/bin:$PATH"
#export JAVA_HOME=$GRAALVM_HOME

# mx
export PATH=~/src/github.com/graalvm/mx:$PATH

# aspectj
export PATH="/opt/aspectj/bin:$PATH"

# antlr
export CLASSPATH=".:/usr/local/lib/antlr-4.7.1-complete.jar:$CLASSPATH"
alias antlr4='java -Xmx500M -cp "/usr/local/lib/antlr-4.7.1-complete.jar:$CLASSPATH" org.antlr.v4.Tool'
alias grun='java -Xmx500M -cp "/usr/local/lib/antlr-4.7.1-complete.jar:$CLASSPATH" org.antlr.v4.gui.TestRig'

# pyls-ms
export PATH="$HOME/.local/share/pyls-ms:$PATH"

# less
if [[ -f "/usr/share/source-highlight/src-hilite-lesspipe.sh" ]]; then
  export LESS='-RMi'
  export LESSOPEN='|  /usr/share/source-highlight/src-hilite-lesspipe.sh %s'
else
  export LESS='-R'
fi

# cabal
export PATH="$HOME/.cabal/bin:$PATH"

# stack
export PATH="$HOME/.stack/bin:$PATH"

# luarocks
export PATH="$HOME/.luarocks/bin:$PATH"

# arandr
export PATH="$HOME/.screenlayout:$PATH"

# XDG settings
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local/share

# xconfig scripts
export PATH="$HOME/.xconfig/bin:$PATH"

_export_pythonpath() {
    project=$1
    if [[ -d $project ]]; then
        export PYTHONPATH="${project}":"${PYTHONPATH}"
    fi
}

# For PyPy and RPython
PYPY_DIR=$HOME/src/foss.heptapod.net/pypy/pypy
RPYTHON=$PYPY_DIR/rpython/bin/rpython
RPLY=$HOME/src/github.com/alex/rply
VMPROF=$HOME/src/github.com/vmprof/vmprof-python
JITVIEWER=$HOME/src/foss.heptapod.net/pypy/jitviewer
CPUSET=$HOME/src/github.com/SUSE/cpuset

if [ -f $HOME/.cargo/env ]; then
    . "$HOME/.cargo/env"
fi
