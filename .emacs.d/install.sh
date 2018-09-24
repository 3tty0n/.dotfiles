#!/bin/bash

echo "Installing emacs lisps..."

if [ ! -d $HOME/.cask ]; then
    curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
fi

cask install

echo "Done."

echo "Installing OCaml environment..."

if [ ! -d $HOME/.opam ]; then
    case "${OSTYPE}" in
        darwin* )
            brew install opam
            ;;
        linux-* )
            sudo apt-get install opam
            ;;
    esac
fi

opam install -y \
    merlin \
    tuareg \
    ocp-indent \
    utop \
    core \
    oUnit \
    user-setup

opam-user-setup install

echo "Done"

echo "Setting up for multi-term..."

curl https://opensource.apple.com/source/emacs/emacs-70/emacs/etc/e/eterm-color.ti\?txt > eterm-color.ti 2>/dev/null
tic -o ~/.terminfo eterm-color.ti 2>/dev/null

echo "Done."
