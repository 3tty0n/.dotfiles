#!/bin/bash

set -eu

DOTFILES_ROOT=$(cd $(dirname $0) && pwd)

usage () {
  echo "Usage:" `basename $0` "[OPTIONS]"
  echo " This script is the set up tool for 3tty0n's environment."
  echo
  echo "Options:"
  echo "  -h  --help         show help"
  echo "  -b  --brew         install brew formulas"
  echo "  -s  --dotfiles     make symbolik links"
  echo "  -z  --zsh          setup zsh plugins managed by zplug"
  echo "  -v  --vim          setup the environment for vim"
  echo "  -e  --emacs        clone 3tty0n/.emacs.d repository"
  echo "  -x                 setup for X window system and XMonad"
  echo "  -a  --all          execute all instructions"
  exit 0
}

function create_symlink {
  printf "makeing symbolik links...\n"

  for f in .zsh .xmonad; do
    ln -sfnv "$DOTFILES_ROOT/$f" "$HOME/$f"
  done

  for f in $(find . -maxdepth 1 -type f -name ".*"); do
    ln -sfnv "$DOTFILES_ROOT/$(basename $f)" "$HOME/$(basename $f)"
  done
}

setup_vim () {
  if [ ! -e ~/.cache/dein ]; then
    curl https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh | bash -s ~/.cache/dein
  fi
}

setup_emacs () {
  if [ ! -e ~/.emacs.d ]; then
    git clone https://github.com/3tty0n/.emacs.d.git ~/.emacs.d
  fi
}

brew_bundle () {
  [ `uname` != "Darwin" ] && exit 0

  [ ! -x "$(which brew)" ] && \
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

  pushd brew
  printf "tapping brew bundle...\n"
  brew tap Homebrew/bundle
  printf "installing brew packages...\n"
  brew bundle
  popd
}

function setup_x {
  for f in .xmonad .Xmodmap .xmobarrc .xsessionrc; do
    ln -sf ~/.dotfiles/$f ~/
  done
}

for OPT in "$@"
do
  case $OPT in
    '-h' | '--help' )
      usage
      exit 1
      ;;
    '-s' | '--dotfiles' )
      create_symlink
      shift 1
      ;;
    '-b' | '--brew' )
      setup_brew
      shift 1
      ;;
    '-v' | '--vim' )
      setup_vim
      shift 1
      ;;
    '-e' | '--emacs' )
      setup_emacs
      shift 1
      ;;
    '-x' )
      setup_x
      shift 1
      ;;
    -*)
      echo "$PROGNAME: illegal option -- '$(echo $1 | sed 's/^-*//')'" 1>&2
      exit 1
      ;;
    *)
      if [[ ! -z "$1" ]] && [[ ! "$1" =~ ^-+ ]]; then
        #param=( ${param[@]} "$1" )
        param+=( "$1" )
        shift 1
      fi
      ;;
  esac
done
