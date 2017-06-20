#!/bin/bash

function install_brew() {
  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
  brew tap Homebrew/bundle
  brew bundle
}

function mk_symlink() {
  declare -a dotfiles=()
  declare -a dotfiles=(".vimrc" ".tmux.conf" ".gitconfig" ".gitignore_global" ".irbrc" ".latexmkrc")

  declare -a vimdir=()
  declare -a vimdir=("ftplugin" "snippets")

  declare -a fishdir=()
  declare -a fishdir=("fish")

  for f in ${dotfiles[@]}; do
    ln -si ~/.dotfiles/${f} ~/${f}
  done

  for v in ${vimdir[@]}; do
    if [ ! -e ~/.vim ]; then
      mkdir ~/.vim
    fi
    ln -si ~/.dotfiles/.vim/${v} ~/.vim/${v}
  done

  ln -si ~/.dotfiles/config/fish ~/.config/fish
}

function install_zprezto() {
  zsh
  git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
  ln -sfi ~/.dotfiles/.zpreztorc ~/.zpreztorc
  ln -sfi ~/.dotfiles/.zshrc ~/.zshrc
}

function install_nonascii_fonts() {
  git clone git@github.com:powerline/fonts.git
  ./fonts/install.sh
  rm -rf fonts
}

function mk_usr_bin() {
  mkdir ~/bin
}

function main() {
  install_brew
  mk_symlink
  mk_usr_bin
  install_zprezto
}

main()
