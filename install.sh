#!/bin/bash

set -eu

DOTFILES_ROOT=$(cd $(dirname $0) && pwd)
PROGNAME="$(basename $0)"

function usage {
  echo "Usage:" `basename $0` "[OPTIONS]"
  echo " This script is the set up tool for 3tty0n's environment."
  echo
  echo "Options:"
  echo "  -h  --help         show help"
  echo "  -d  --dotfiles     plase configuration files as a symbolik link"
  echo "  -e  --emacs        clone 3tty0n/.emacs.d repository"
  echo "  -X                 clone 3tty0n/xconfig repository"
  echo "  -D                 execute as a debug mode"
  echo "  -a --all           set up all "
  exit 0
}

function setup_dotfiles {
  find . -maxdepth 1 -type f -name ".*" -not -name ".gitignore" \
       -exec ln -sfv "$DOTFILES_ROOT/$(basename {})" "$HOME/$(basename {})" ';'

  find .config -type f -exec ln -sfnv $DOTFILES_ROOT/{} $HOME/{} ';'
  ln -sfnv "$DOTFILES_ROOT/.zsh" "$HOME/.zsh"
}

function setup_emacs {
  if [ ! -e ~/.emacs.d ]; then
    printf "cloning .emacs.d dir...\n"
    git clone git@github.com:3tty0n/.emacs.d.git ~/.emacs.d 2>/dev/null
    printf "done.\n"
  fi
}

function setup_xconfig {
  if [ "$(uname)" = "Linux" ] && [ ! -e ~/.xconfig ]; then
    printf "cloning xconfig dir...\n"
    git clone git@github.com:3tty0n/.xconfig.git ~/.xconfig 2>/dev/null
    printf "done.\n"
  fi
}

function setup_email {
  if [ ! -e ~/.xmail ]; then
    printf "cloning xmail dir...\n"
    git clone git@github.com:3tty0n/.xmail.git ~/.xmail 2>/dev/null
    printf "done.\n"
  fi
}

for OPT in "$@"; do
  case $OPT in
    '-h' | '--help' ) usage; exit 1 ;;
    '-d' | '--dotfiles' ) setup_dotfiles; shift 1 ;;
    '-e' | '--emacs' ) setup_emacs; shift 1 ;;
    '-m' | '--mail' ) setup_email; shift 1 ;;
    '-a' | '--all' ) setup_dotfiles; setup_emacs; setup_xconfig; setup_email; exit ;;
    '-D' ) set -x; shift 1 ;;
    -*) echo "$PROGNAME: illegal option -- '$(echo $1 | sed 's/^-*//')'" 1>&2; exit 1 ;;
    *)
      if [[ ! -z "$1" ]] && [[ ! "$1" =~ ^-+ ]]; then
        param+=( "$1" )
        shift 1
      fi
      ;;
  esac
done
