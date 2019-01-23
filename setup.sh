#!/bin/zsh

DOTFILES_ROOT=$(cd $(dirname $0) && pwd)

declare -a dotfiles=()
declare -a dotfiles=(
    .vimrc
    .vimrc.local.vim
    .tmux.conf
    .gitconfig
    .gitignore_global
    .irbrc
    .latexmkrc
    .gemrc
    .zshenv
    .zshrc
    .zprofile
    .zsh
    .tigrc
    .sbtrc
    .spacemacs
    .utoprc
    .SpaceVim.d
)

declare -a vimfiles=()
declare -a vimfiles=(ftplugin snippets)

declare -a configfiles=()
declare -a configfiles=(fish powerline)

declare -a ghqrepos=()
declare -a ghqrepos=(bahlo/iterm-colors t3chnoboy/thayer-bright-iTerm)


usage () {
  echo "Usage:" `basename $0` "[OPTIONS]"
  echo " This script is the set up tool for 3tty0n's environment."
  echo
  echo "Options:"
  echo "  -h          show help"
  echo "  -b          install brew formulas"
  echo "  -s          make symbolik links"
  echo "  -z          setup zsh plugins managed by zplug"
  echo "  -g          get github repo that is needed in my environment"
  echo "  -a          execute all instructions"
  exit 0
}

create_symlink () {
  printf "makeing symbolik links...\n"
  {
    for f in ${dotfiles[@]}; do
      ln -sfnv "$DOTFILES_ROOT/$f" "$HOME/$f"
    done

    for c in ${configfiles[@]}; do
      ln -sfnv "$DOTFILES_ROOT/.config/$c" "$HOME/.config/$c"
    done
  }>/dev/null

}

setup_zplug () {
  if [ ! -e ~/.zplug ]; then
    curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh| zsh
  fi
}

setup_dein () {
  if [ ! -e ~/.cache/dein ]; then
    curl https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh | bash -s ~/.cache/dein
  fi
}

brew_bundle () {
  if [ ! -x "$(which brew)" ]; then
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
  fi
  cd brew || exit
  printf "tapping brew bundle...\n"
  (brew tap Homebrew/bundle)>/dev/null
  printf "installing brew packages...\n"
  (brew bundle)>/dev/null
}

for OPT in "$@"
do
  case $OPT in
    '-h' )
      usage
      ;;
    '-s' )
      create_symlink
      ;;
    '-b' )
      brew_bundle
      ;;
    '-z' )
      setup_zplug
      ;;
    '-d' )
      setup_dein
      ;;
    '-a' )
      create_symlink
      setup_zplug
      brew_bundle
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
  esac
  shift
done
