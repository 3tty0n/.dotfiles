type fzy >/dev/null 2>&1 && j() {
  local recentd
  z -l | tail -r | awk '{ print $2 }' | fzy | read recentd && cd $recentd
}
