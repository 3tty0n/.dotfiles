#!/bin/sh

case "${OSTYPE}" in
  darwin* )
    brew tap sanemat/font
    brew install ricty
    cp -f /usr/local/Cellar/ricty/4.0.1/share/fonts/Ricty*.ttf ~/Library/Fonts/
    fc-cache -vf
    ;;
esac
