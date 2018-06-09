export SELECTER=fzf

function golf() {
  gol ls | fzf | awk -F '[:]' '{print $2 ":" $3}' | xargs open
}
