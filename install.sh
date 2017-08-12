#!/bin/zsh
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

spin (){
  pid=$! # Process Id of the previous running command

  spin='-\|/'

  i=0
  while kill -0 $pid 2>/dev/null
  do
    i=$(( (i+1) %4 ))
    printf "\r${spin:$i:1}"
    sleep .1
  done
}

install () {
  if [ ! -d ~/.dotfiles ];then
    git clone git@github.com:3tty0n/.dotfiles.git ~/.dotfiles &/dev/null
  fi
  git checkout $BRANCH &>/dev/null
  ./setup.sh &>/dev/null
  ./brewfile.sh &>/dev/null
}

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
  printf " Install in $BRANCH branch ...\n"
  install & spin && printf "\n"
  printf " All processes are successfully completed \U1F389\n"
  printf " For more information, see ${(%):-%U}https://github.com/3tty0n/.dotfiles${(%):-%u} \U1F33A\n"
  printf " Enjoy hacking!\n"
fi
