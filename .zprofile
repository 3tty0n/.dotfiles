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
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"

# xwindow
if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  exec startx
fi
