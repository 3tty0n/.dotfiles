#!/bin/zsh

fadd() {
  local addfiles
  addfiles=($(git status --short | awk '{ print $2 }' | fzf --multi))
  if [[ -n $addfiles ]]; then
    git add ${@:1} $addfiles && echo "added: $addfiles"
  else
    echo "nothing added."
  fi
}
