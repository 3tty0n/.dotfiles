#!/bin/bash

DOTFILES_ROOT=~/.dotfiles

declare -a dotfiles=()
declare -a dotfiles=(.vimrc .tmux.conf .gitconfig .gitignore_global .irbrc .latexmkrc .gemrc .zshrc .zsh .vimrc.local.vim bin)

for f in ${dotfiles[@]}; do
  ln -sfnv $DOTFILES_ROOT/${f} ~/${f}
done

[ ! -e ~/.vim ] && mkdir ~/.vim
for vimfile in ftplugin snippets; do
  ln -sfnv $DOTFILES_ROOT/$vimfile ~/.vim/$vimfile
done

[ ! -e ~/.config/fish ] && mkdir -p ~/.config/fish
ln -sfnv $DOTFILES_ROOT/.config/fish/config.fish ~/.config/fish

[ ! -e ~/.config/gist ] && mkdir -p ~/.config/gist
# ln -sfnv $DOTFILES_ROOT/.config/gist/config.toml ~/.config/gist

if [ ! -e ~/.zprezto ]; then
  git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
fi

if [ ! -e ~/.zplug ]; then
  curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh| zsh
fi
