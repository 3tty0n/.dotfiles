#!/bin/sh

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
