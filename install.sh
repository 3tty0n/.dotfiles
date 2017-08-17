#!/bin/zsh
set -eu
autoload -Uz colors; colors
typeset TMPFILE="/tmp/.spin-$$$RANDOM"
BRANCH=master

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

move() {
  col="$1"
  line="$2"
  printf "\r\033[${line};${col}f"
}

eraceCurrentLine() {
  printf "\033[2K\r"
}

get_line() {
  echo -ne "\r\033[6n"
  read -s -d\[ garbage
  read -s -d R foo
  REPLY=$(echo "$foo" | sed 's/;.*$//')
}

sp() {
  local    spinner
  local -a spinners
  spinners=(⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏)

  #line
  #line=$REPLY
  line=$1

  sleep ${3:-1} & id=$!

  while [[ $jobstates =~ $id ]]
  do
    for spinner in "${spinners[@]}"
    do
      sleep 0.05
      printf "\r\033[$line;1f $fg[white]$spinner$reset_color  Installing...  $2" 2>/dev/null
      [[ $jobstates =~ $id ]] || break
    done
  done

  move 1 $line
  eraceCurrentLine
  printf " $fg_bold[blue]\U2714$reset_color  $fg[green]Installed!$reset_color     $2\n"
}

install () {
  { if [ ! -d ~/.dotfiles ];then
      git clone git@github.com:3tty0n/.dotfiles.git ~/.dotfiles
    fi
    git checkout $BRANCH && setup && brewfile
  }  &>/dev/null
}

trap 'echo ' {1,2,3,15}

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

(
  printf "\n\n"
  # hide cursor
  tput civis

  get_line
  line=$REPLY
  install & { sp $(( line+=0 )) "${BRANCH}"}

  # show cursor
  tput cnorm
  printf "\n\n"
  printf " All processes are successfully completed \U1F389\n"
  printf " For more information, see ${(%):-%U}https://github.com/3tty0n/.dotfiles${(%):-%u} \U1F33A\n"
  printf " Enjoy hacking!\n"
) || {
  printf "\033[2K" 2>/dev/null
  printf "Oops \U2620 ... Try again!\n" 2>/dev/null
  exit 1
}
