#!/bin/sh
set -x

find . -maxdepth 1 -type f -name ".*" -not -name ".gitignore" \
     -exec rm -f $HOME/{} ';'
