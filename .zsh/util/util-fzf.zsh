#!/bin/zsh

fsn () {
  local TEMPLATE="$(curl https://github.com/foundweekends/giter8/wiki/giter8-templates -s | grep "\.g8<" | sed -e "s/</ /g" -e "s/>/ /g" | awk '{print $3}' | fzf | head -n 1)"
  if [[ -z "$TEMPLATE" ]]; then
    return
  fi
  sbt new $TEMPLATE
  rm -rf target || exit 1
}

fco() {
  local tags branches target
  tags=$(
    git tag | awk '{print "\x1b[31;1mtag\x1b[m\t" $1}') || return
  branches=$(
    git branch --all | grep -v HEAD             |
    sed "s/.* //"    | sed "s#remotes/[^/]*/##" |
    sort -u          | awk '{print "\x1b[34;1mbranch\x1b[m\t" $1}') || return
  target=$(
    (echo "$tags"; echo "$branches") |
    fzf-tmux -l30 -- --no-hscroll --ansi +m -d "\t" -n 2) || return
  git checkout $(echo "$target" | awk '{print $2}')
}

fbr() {
  local branches branch
  branches=$(git for-each-ref --count=30 --sort=-committerdate refs/heads/ --format="%(refname:short)") &&
  branch=$(echo "$branches" |
           fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
  git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}

fbrd () {
  git branch | fzf | xargs git branch -D || zle reset-prompt
}
