#!/bin/zsh

DOTFILES_ROOT=$(cd $(dirname $0) && pwd)

declare -a dotfiles=()
declare -a dotfiles=(
    .vimrc
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
    .ocamlinit
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
  echo "  -h  --help         show help"
  echo "  -b  --brew         install brew formulas"
  echo "  -d  --dotfiles     make symbolik links"
  echo "  -z  --zsh          setup zsh plugins managed by zplug"
  echo "  -v  --vim          setup the environment for vim"
  echo "  -e  --emacs        clone 3tty0n/.emacs.d repository"
  echo "  -a  --all          execute all instructions"
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
    } >/dev/null
}

setup_zsh () {
    if [ ! -e ~/.zplug ]; then
        curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh| zsh
    fi
}

setup_tmux () {
    ln -sf ~/.dotfiles/.tmux-powerlinerc ~/
    if [ ! -e ~/.tmux/tmux-powerline ]; then
        git clone https://github.com/erikw/tmux-powerline.git ~/.tmux/tmux-powerline
    fi
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
    if [ ! -x "$(which brew)" ]; then
        /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    fi
    pushd brew
    printf "tapping brew bundle...\n"
    (brew tap Homebrew/bundle)>/dev/null
    printf "installing brew packages...\n"
    (brew bundle)>/dev/null
    popd
}

TMUX_FLG=false
EMACS_FLG=false
VIM_FLG=false
BREW_FLG=false
ZSH_FLG=false
DOTFILES_FLG=false

make_all_flgs_true () {
    TMUX_FLG=true
    EMACS_FLG=true
    VIM_FLG=true
    BREW_FLG=true
    ZSH_FLG=true
    DOTFILES_FLG=true
}

for OPT in "$@"
do
    case $OPT in
        '-h' | '--help' )
            usage
            exit 1
            ;;
        '-s' | '--dotfiles' )
            DOTFILES_FLG=true
            shift 1
            ;;
        '-b' | '--brew' )
            BREW_FLG=true
            shift 1
            ;;
        '-z' | '--zsh' )
            ZSH_FLG=true
            shift 1
            ;;
        '-v' | '--vim' )
            VIM_FLG=true
            ;;
        '-e' | '--emacs' )
            EMACS_FLG=true
            ;;
        '-t' | '--tmux' )
            TMUX_FLG=true
            ;;
        '-a' | '--all' )
            make_all_flgs_true
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

$TMUX_FLG && setup_tmux
$EMACS_FLG && setup_emacs
$VIM_FLG && setup_vim
$BREW_FLG && setup_brew
$ZSH_FLG && setup_zplug
$DOTFILES_FLG && create_symlink
