typeset -gx -U path
fpath=(${HOME}/.zsh/completion ${fpath})
# autoload -Uz compinit && compinit -i

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

# less
export LESS='-gj10 --no-init --quit-if-one-screen --RAW-CONTROL-CHARS'
# export LESSSOPEN='| /usr/local/bin/src-hilite-lesspipe.sh %s'

# grep
# export GREP_OPTIONS='--color=always'
# export GREP_COLOR='1;35;40'

# ls command colors
export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

# fzf
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --ansi --no-bold'

# Add ~/usr/local/bin to PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=/usr/local/bin:$PATH

# pyenv
export PYENV_ROOT=$HOME/.pyenv
export PATH="$PYENV_ROOT/bin:$PATH"

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

# graal
GRAALVM_HOME=$HOME/.local/share/graalvm-ce-java11-21.2.0
if [ -d "${GRAALVM_HOME}" ]; then
  export GRAALVM_HOME
  export PATH="${GRAALVM_HOME}/bin:$PATH"
  export JAVA_HOME="${GRAALVMHOME}"
fi

# mx
export PATH="~/.local/share/mx:$PATH"

# jdk
export JAVA_HOME=/usr/lib/jvm/java-11-openjdk

# aspectj
export PATH="/opt/aspectj/bin:$PATH"

# antlr
export CLASSPATH=".:/usr/local/lib/antlr-4.7.1-complete.jar:$CLASSPATH"
alias antlr4='java -Xmx500M -cp "/usr/local/lib/antlr-4.7.1-complete.jar:$CLASSPATH" org.antlr.v4.Tool'
alias grun='java -Xmx500M -cp "/usr/local/lib/antlr-4.7.1-complete.jar:$CLASSPATH" org.antlr.v4.gui.TestRig'

# pyls-ms
export PATH="$HOME/.local/share/pyls-ms:$PATH"

# less
if [[ "`which src-highlight-lesspipe.sh`" ]]; then
  export LESS='-RMi'
  export LESSOPEN='| /usr/local/bin/src-hilite-lesspipe.sh %s'
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
PYPY=$HOME/src/foss.heptapod.net/pypy/pypy
RPLY=$HOME/src/github.com/alex/rply
VMPROF=$HOME/src/github.com/vmprof/vmprof-python
if [[ -d $PYPY ]]; then
    for proj in $PYPY $RPLY $VMPROF; do
        _export_pythonpath $proj
    done
    export PATH=$PYPY/rpython/bin:$PATH
fi
