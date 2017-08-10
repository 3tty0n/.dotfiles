#!/bin/bash
set -eu

is_master=false
is_develop=false

BRANCH=""

usage () {
  echo "Usage:" `basename $0` "[OPTIONS]"
  echo " This script is the installer for 3tty0n's environment."
  echo
  echo "Options:"
  echo "  -h, --help"
  echo "  -m, --master"
  echo "  -d, --develop"
  echo
  exit 1
}

exec_script () {
  ./setup.sh
  ./brewfile.sh
}

install () {
  git clone git@github.com:3tty0n/.dotfiles.git ~/.dotfiles && cd ~/.dotfiles
  git checkout $BRANCH
  exec_script
}

# [[ -d ${HOME}/.dotfiles ]] && mv ${HOME}/.dotfiles ${HOME}/.dotfiles.orig

for OPT in "$@"; do
  case "$OPT" in
    '-h'| '--help' )
      usage
      exit 1
      ;;
    '-m' | '--master' )
      BRANCH=master
      shift 1
      ;;
    '-d' | '--develop' )
      BRANCH=develop
      shift 1
      ;;
    '--'|'-' )
      shift 1
      param+=( "$@" )
      break
      ;;
    -*)
      echo "`basename $0`: illegal option -- '$(echo $1 | sed 's/^-*//')'" 1>&2
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

if [ -z "$BRANCH" ]; then
  usage
else
  echo "Install in $BRANCH branch ..."
  echo
  install
  echo "installation is finished."
  echo
fi
