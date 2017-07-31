#!/bin/bash

if [ ! -x "`which brew`" ]; then
  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi
cp ./brew/Brewfile .
brew tap Homebrew/bundle
brew bundle
rm -f ./Brewfile
