#!/bin/bash

git clone --recursive git@github.com:3tty0n/.dotfiles.git ~/.dotfiles
cd ~/.dotfiles
git checkout master
./setup.sh
./brewfile.sh
