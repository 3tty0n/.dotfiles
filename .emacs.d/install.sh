#!/bin/bash

deps=(
    merlin
    tuareg
    ocp-indent
    utop
    dune
    core
    menhir
    stringext
    ppx_deriving)

echo "Installing emacs lisps..."

if [ ! -d $HOME/.cask ]; then
    curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
fi

cask install

echo "Done."

echo "setting up for ricty font..."

if [ ! -x "$(which brew)"]; then
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi
brew tap sanemat/font
brew install Caskroom/cask/xquartz
brew install ricty --with-powerline

cp -f /usr/local/opt/ricty/share/fonts/Ricty*.ttf ~/Library/Fonts/
fc-cache -vf

echo "Done."

echo "setting up for multi-term..."

curl https://opensource.apple.com/source/emacs/emacs-70/emacs/etc/e/eterm-color.ti\?txt > eterm-color.ti 2>/dev/null
tic -o ~/.terminfo eterm-color.ti 2>/dev/null

echo "Done."

echo "installing ocaml environment..."

if [ ! -d $home/.opam ]; then
    case "${ostype}" in
        darwin* )
            brew install opam
            ;;
        linux-* )
            sudo apt-get install opam
            ;;
    esac
fi

opam install -y ${deps[@]}

echo "done"
