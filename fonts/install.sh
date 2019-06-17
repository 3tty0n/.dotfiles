#!/bin/bash

VERSION=0.1

CONSOLAS_FLG=false
POWERLINE_FLG=false
RICTY_FLG=false
NERD_FLG=false

install_consolas () {
    echo "Installing consolas fonts..."
    case "${OSTYPE}" in
        darwin* )
            brew install cabextract
            cd ~/Downloads
            mkdir consolas
            cd consolas
            curl -LO https://sourceforge.net/projects/mscorefonts2/files/cabs/PowerPointViewer.exe
            cabextract PowerPointViewer.exe
            cabextract ppviewer.cab
            open CONSOLA*.TTF
            ;;
        * )
            echo "This script is written for macOS."
            ;;
    esac
}

install_powerline () {
    echo "Installing powerline fonts..."
    # clone
    git clone https://github.com/powerline/fonts.git --depth=1
    # install
    pushd fonts
    ./install.sh
    # clean-up a bit
    popd
    rm -rf fonts
}

install_ricty () {
    echo "Installing ricty fonts..."
    case "${OSTYPE}" in
        darwin* )
            brew tap sanemat/font
            brew install ricty
            cp -f /usr/local/share/fonts/Ricty*.ttf ~/Library/Fonts
            fc-cache -vf
            ;;
        linux* )
            sudo apt install fonts-ricty-diminished  
            fc-cache -fv
            ;;
    esac
}

install_nerd () {
    echo "Installing nerd fonts..."
    case "${OSTYPE}" in
        darwin* )
            brew tap caskroom/fonts
            brew cask install font-hack-nerd-font
            ;;
    esac
}

for OPT in "$@"
do
    case "$OPT" in
        '-h'|'--help' )
            usage
            exit 1
            ;;
        '--version' )
            echo $VERSION
            exit 1
            ;;
        '--consolas' )
            CONSOLAS_FLG=true
            shift 1
            ;;
        '--ricty' )
            RICTY_FLG=true
            shift 1
            ;;
        '--powerline' )
            POWERLINE_FLG=true
            shift 1
            ;;
        '--nerd' )
            NERD_FLG=true
            shift 1
            ;;
        '--all')
            RICTY_FLG=true
            POWERLINE_FLG=true
            NERD_FLG=true
            CONSOLAS_FLG=true
            shift 1
            ;;
        '--'|'-' )
            shift 1
            param+=( "$@" )
            break
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

$CONSOLAS_FLG && install_consolas
$RICTY_FLG && install_ricty
$NERD_FLG && install_nerd
$POWERLINE_FLG && install_powerline
