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
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init --path)"
fi

# xwindow
if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  exec startx
fi
