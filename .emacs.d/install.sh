d#!/bin/bash

if [ ! -d ~/.emacs.d ]; then
  git clone --recursive git@github.com:3tty0n/.emacs.d.git ~/.emacs.d
  cd ~/.emacs.d || exit
fi

if [ ! -x brew ]; then
  /usr/bin/ruby -e \
    "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

if [ ! -d site-lisp/PG ]; then
  git clone https://github.com/ProofGeneral/PG ~/.emacs.d/site-lisp/PG
  cd ~/.emacs.d/site-lisp/PG || exit
  make
fi

brew install emacs cask opam merlin && \
  brew install cmigemo --HEAD && \
  cask install --verbose && \
  pip install jedi virtualenv
