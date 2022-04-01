[[ ! -f ~/.profile ]] || source ~/.profile

# scalaenv
if [ -x "`which scalaenv`" ]; then
  eval "$(scalaenv init - --no-rehash)"
fi

# direnv
if [ -x "`which direnv`" ]; then
  eval "$(direnv hook zsh)"
fi

# opam
if [ -x "`which opam`" ]; then
  eval $(opam config env)
fi

export PYENV_ROOT="$HOME/.pyenv"

if [ -d ${PYENV_ROOT} ]; then
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init --path)"
fi

# xwindow
if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  exec startx
fi
