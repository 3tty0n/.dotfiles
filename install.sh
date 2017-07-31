#!/bin/bash

DOTFILES_ROOT=~/.dotfiles

function install_brew_packages() {
  if [ ! -e /usr/local/Cellar ]; then
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
  fi

  cp ./brew/Brewfile .
  brew tap Homebrew/bundle
  brew bundle
  rm Brewfile
}

function install() {

  declare -a dotfiles=()
  declare -a dotfiles=(.vimrc .tmux.conf .gitconfig .gitignore_global .irbrc .latexmkrc .gemrc .zshrc .zsh .vimrc.local.vim bin)

  for f in ${dotfiles[@]}; do
    ln -sfn $DOTFILES_ROOT/${f} ~/${f}
  done

  [ ! -e ~/.vim ] && mkdir ~/.vim
  for vimfile in ftplugin snippets; do
    ln -sfn $DOTFILES_ROOT/$vimfile ~/.vim/$vimfile
  done

  if [ ! -e ~/.zprezto ]; then
    git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
  fi

  if [ ! -e ~/.zplug ]; then
    curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh| zsh
  fi
}

install
# install_brew_packages
