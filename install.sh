#!/bin/bash

function install_brew_packages() {
  cp ./brew/Brewfile .
  brew tap Homebrew/bundle
  brew bundle
  rm Brewfile
}

function install() {
  if [ ! -e /usr/local/Cellar ]; then
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
  fi

  if [ ! -e ~/bin ]; then
    mkdir ~/bin
  fi

  declare -a dotfiles=()
  declare -a dotfiles=(".vimrc" ".tmux.conf" ".gitconfig" ".gitignore_global" ".irbrc" ".latexmkrc" ".gemrc" ".zshrc" ".vimrc.local.vim")

  for f in ${dotfiles[@]}; do
    ln -si ~/.dotfiles/${f} ~/${f}
  done

  if [ ! -e ~/.zsh ]; then
    ln -si ~/.dotfiles/.zsh ~/.zsh
  fi

  if [ ! -e ~/.vim ]; then
    mkdir ~/.vim
  fi
  ln -si ~/.dotfiles/.vim/ftplugin ~/.vim/ftplugin
  ln -si ~/.dotfiles/.vim/snippets ~/.vim/snippets

  if [ ! -e ~/.zprezto ]; then
    git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
  fi

  if [ ! -e ~/.zplug ]; then
    curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh| zsh
  fi
}

install
# install_brew_packages
