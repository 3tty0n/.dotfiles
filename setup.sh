#!/bin/bash

DOTFILES_ROOT=~/.dotfiles

declare -a dotfiles=()
declare -a dotfiles=(.vimrc .tmux.conf .gitconfig .gitignore_global .irbrc .latexmkrc .gemrc .zshenv .zshrc .zsh .vimrc.local.vim bin .tigrc)

declare -a vimfiles=()
declare -a vimfiles=(ftplugin snippets)

declare -a configfiles=()
declare -a configfiles=(fish omf)

declare -a ghqrepos=()
declare -a ghqrepos=(banga/powerline-shell bahlo/iterm-colors t3chnoboy/thayer-bright-iTerm tarjoilija/zgen)

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

if [ ! -x "`which ghq`" ]; then
  if [ ! -x "`which go`" ]; then
    echo "Please install go."
    exit 1
  fi
  echo "Install ghq ..."
  go get github.com/motemen/ghq
  echo "Installation is finished. Please re-run this script."
  exit 1
fi

for repo in ${ghqrepos[@]}; do
  if [ ! -d `ghq root`/github.com/$repo ]; then
    ghq get https://github.com/$repo
  fi
done
