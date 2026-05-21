# Minimal zsh plugin framework
#
# Usage:
#   plugin <user/repo|URL>          install (if missing) and source
#   plugin --fpath <user/repo|URL> [subdir]
#                                   install and add dir (or dir/subdir) to fpath; no source
#   plugin_update                   git-pull every installed plugin

typeset -g _PLUGIN_DIR="${ZSH_PLUGIN_DIR:-$HOME/.zsh/plugins}"
mkdir -p "$_PLUGIN_DIR"

plugin() {
  local fpath_only=0 spec subdir name url dir

  [[ $1 == --fpath ]] && { fpath_only=1; shift }
  spec=$1
  subdir=${2:-}

  # Resolve URL and bare name from "user/repo" shorthand or full URL
  if [[ $spec == *://* ]]; then
    url=$spec
    name=${${spec%.git}:t}
  else
    url="https://github.com/${spec}.git"
    name=${spec:t}
  fi

  dir="$_PLUGIN_DIR/$name"

  # Install if missing
  if [[ ! -d $dir ]]; then
    print -r "plugin: installing ${name}..." >&2
    git clone --depth=1 --quiet "$url" "$dir" \
      || { print -r "plugin: clone failed for ${name}" >&2; return 1 }
  fi

  # --fpath: add directory (or subdirectory) to fpath, do not source
  if (( fpath_only )); then
    local target="${subdir:+$dir/$subdir}"
    target="${target:-$dir}"
    [[ -d $target ]] && fpath=("$target" $fpath)
    return
  fi

  # Source: find and load the plugin's entry point
  local f
  for f in \
    "$dir/${name}.plugin.zsh" \
    "$dir/${name}.zsh"        \
    "$dir/init.zsh"           \
    "$dir/${name}.sh"; do
    [[ -f $f ]] && { source "$f"; return }
  done

  print -r "plugin: no entry point found in ${name}" >&2
  return 1
}

# git-pull every directory under _PLUGIN_DIR
plugin_update() {
  local d
  for d in "$_PLUGIN_DIR"/*(N/); do
    print -r "plugin: updating ${d:t}..."
    git -C "$d" pull --ff-only --quiet
  done
}
