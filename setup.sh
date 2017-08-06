#!/bin/bash

DOTFILES_ROOT=~/.dotfiles

declare -a dotfiles=()
declare -a dotfiles=(.vimrc .tmux.conf .gitconfig .gitignore_global .irbrc .latexmkrc .gemrc .zshenv .zshrc .zsh .vimrc.local.vim bin .tigrc)

declare -a vimfiles=()
declare -a vimfiles=(ftplugin snippets)

declare -a configfiles=()
declare -a configfiles=(fish omf)

for f in ${dotfiles[@]}; do
  ln -sfnv $DOTFILES_ROOT/${f} ~/${f}
done

[ ! -e ~/.vim ] && mkdir ~/.vim
for vimfile in ${vimfiles[@]}; do
  ln -sfnv $DOTFILES_ROOT/$vimfile ~/.vim/$vimfile
done

#for config in ${configfiles[@]}; do; ln -sfnv $DOTFILES_ROOT/.config/$config ~/.config; done

if [ ! -e ~/.config/gist ]; then
  mkdir -p ~/.config/gist
  cp -v $DOTFILES_ROOT/.config/gist/config.toml ~/.config/gist
fi

if [ ! -e ~/.zprezto ]; then
  git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
fi
