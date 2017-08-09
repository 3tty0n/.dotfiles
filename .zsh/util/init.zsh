#!/bin/zsh

WORK_DIR=$(chdir $(dirname $0) && pwd)
for file in $WORK_DIR/src/*.zsh; do
  source $file
done
