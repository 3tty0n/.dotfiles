# Minimal zsh plugin framework
#
# Usage:
#   plugin <user/repo|URL>              install (if missing) and source
#   plugin --fpath <user/repo|URL> [subdir]
#                                       install and add dir to fpath; no source
#   plugin --binary <user/repo> [pick] [bpick]
#                                       install binary from GitHub release
#                                       (OS/arch auto-detected via uname)
#   plugin update [name ...]            git-pull (or re-fetch binary) plugins
#   plugin delete <name ...>            remove installed plugins
#   plugin cleanup [--dry-run]          remove plugins not listed in ZSH_PLUGIN_RC

typeset -g _PLUGIN_DIR _PLUGIN_RC
_PLUGIN_DIR="${ZSH_PLUGIN_DIR:-$HOME/.zsh/plugins}"
_PLUGIN_RC="${ZSH_PLUGIN_RC:-$HOME/.zshrc.mini}"
setopt extended_glob
mkdir -p "$_PLUGIN_DIR"

_plugin_dir() {
  _PLUGIN_DIR="${ZSH_PLUGIN_DIR:-$HOME/.zsh/plugins}"
  print -r -- "$_PLUGIN_DIR"
}

_plugin_rc() {
  print -r -- "${ZSH_PLUGIN_RC:-$_PLUGIN_RC}"
}

_plugin_resolve_name() {
  local spec=$1
  if [[ $spec == *://* ]]; then
    print -r -- ${${spec%.git}:t}
  else
    print -r -- ${spec:t}
  fi
}

_plugin_resolve_repo() {
  local spec=$1
  if [[ $spec == *://* ]]; then
    local path=${${spec%.git}#*://*/}
    print -r -- ${path#github.com/}
  else
    print -r -- $spec
  fi
}

_plugin_host_os() {
  case "$(uname -s)" in
    Darwin)  print -r darwin ;;
    Linux)   print -r linux ;;
    FreeBSD) print -r freebsd ;;
    *)       return 1 ;;
  esac
}

_plugin_host_arch() {
  case "$(uname -m)" in
    arm64|aarch64) print -r arm64 ;;
    x86_64|amd64)  print -r amd64 ;;
    i386|i686)     print -r 386 ;;
    *)             return 1 ;;
  esac
}

# fnmatch patterns for GitHub release assets on this host (most specific first).
_plugin_bpick_candidates() {
  local os arch
  local -a arch_tokens candidates
  os=$(_plugin_host_os) || return 1
  arch=$(_plugin_host_arch) || return 1

  case $arch in
    amd64) arch_tokens=(amd64 x86_64 x64 amd) ;;
    arm64) arch_tokens=(arm64 aarch64 arm) ;;
    386)   arch_tokens=(386 i386 i686) ;;
    *)     arch_tokens=($arch) ;;
  esac

  local a
  for a in $arch_tokens; do
    candidates+=("*${os}_${a}*")
    candidates+=("*${os}*${a}*")
  done
  print -lr ${(u)candidates}
}

# Return download URL; sets _PLUGIN_LAST_BPICK to the matched pattern.
_plugin_gh_asset_match() {
  local repo=$1
  shift
  local -a patterns result
  patterns=("$@")
  (( ${#patterns} )) || patterns=($(_plugin_bpick_candidates)) || return 1

  result=("${(@f)$(python3 - "$repo" "${patterns[@]}" <<'PY'
import fnmatch, json, sys, urllib.request

repo = sys.argv[1]
patterns = sys.argv[2:]
skip = ("checksum", "sha256", "shasum", "sig", ".txt", ".md")
req = urllib.request.Request(
    f"https://api.github.com/repos/{repo}/releases/latest",
    headers={"Accept": "application/vnd.github+json"},
)
with urllib.request.urlopen(req) as resp:
    data = json.load(resp)
for pat in patterns:
    for asset in data.get("assets", []):
        name = asset["name"]
        lower = name.lower()
        if any(s in lower for s in skip):
            continue
        if fnmatch.fnmatch(lower, pat.lower()):
            print(asset["browser_download_url"])
            print(pat)
            raise SystemExit(0)
raise SystemExit(1)
PY
)}")
  [[ ${#result} -ge 2 ]] || return 1
  typeset -g _PLUGIN_LAST_BPICK=$result[2]
  print -r -- "$result[1]"
}

_plugin_path_add() {
  local bindir=$1
  [[ -d $bindir ]] || return
  (( ${path[(Ie)$bindir]} )) || path=($bindir $path)
  export PATH="${(j.:.)path}"
}

_plugin_register_binary_paths() {
  local d bindir
  for d in "$(_plugin_dir)"/*(N/); do
    [[ -f $d/.plugin-meta ]] || continue
    bindir=$d/bin
    _plugin_path_add "$bindir"
  done
}

_plugin_extract_root() {
  local base=$1
  python3 - "$base" <<'PY'
import os, sys

base = sys.argv[1]
if not os.path.isdir(base):
    print(base)
    raise SystemExit(0)

entries = [os.path.join(base, name) for name in os.listdir(base)]
dirs = [p for p in entries if os.path.isdir(p)]
files = [p for p in entries if os.path.isfile(p)]

if len(dirs) == 1:
    print(dirs[0])
elif len(entries) == 1 and len(files) == 1:
    print(base)
elif len(entries) == 1:
    print(entries[0])
else:
    print(base)
PY
}

_plugin_fetch_repo_subdir() {
  local repo=$1 subdir=$2 dest=$3
  local tmp
  tmp=$(mktemp -d)
  git clone --depth=1 --filter=blob:none --sparse "https://github.com/${repo}.git" "$tmp/repo" 2>/dev/null &&
    git -C "$tmp/repo" sparse-checkout set "$subdir" 2>/dev/null &&
    cp -R "$tmp/repo/$subdir" "$dest"
  rm -rf "$tmp"
}

_plugin_extract_release() {
  local archive=$1 dest=$2
  mkdir -p "$dest"
  case ${archive:t} in
    *.tar.gz|*.tgz) tar -xzf "$archive" -C "$dest" ;;
    *.tar.xz|*.txz) tar -xJf "$archive" -C "$dest" ;;
    *.zip)          unzip -qq -o "$archive" -d "$dest" ;;
    *)
      if file -b "$archive" | grep -qi 'zip'; then
        unzip -qq -o "$archive" -d "$dest"
      elif file -b "$archive" | grep -qi 'gzip'; then
        tar -xzf "$archive" -C "$dest"
      elif file -b "$archive" | grep -qi 'xz'; then
        tar -xJf "$archive" -C "$dest"
      else
        cp "$archive" "$dest/extracted"
      fi
      ;;
  esac
}

_plugin_pick_binary() {
  local root=$1 pick=$2
  python3 - "$root" "$pick" <<'PY'
import fnmatch, os, sys

root, pick = sys.argv[1], sys.argv[2]
patterns = [pick]
if pick.startswith("*/"):
    patterns.append(pick[2:])
if "/" in pick:
    patterns.append(pick.rsplit("/", 1)[-1])
seen = set()
matches = []
for pattern in patterns:
    if pattern in seen:
        continue
    seen.add(pattern)
    for dirpath, _, files in os.walk(root):
        for name in files:
            path = os.path.join(dirpath, name)
            rel = os.path.relpath(path, root)
            if fnmatch.fnmatch(rel, pattern) or fnmatch.fnmatch(name, pattern):
                matches.append(path)
if not matches:
    raise SystemExit(1)
for path in matches:
    if os.access(path, os.X_OK):
        print(path)
        raise SystemExit(0)
print(matches[0])
PY
}

plugin_binary() {
  local spec=$1 pick=${2:-} bpick=${3:-}
  local name repo dir url tmp
  local -a patterns
  local content os arch
  local installing=0
  name=$(_plugin_resolve_name "$spec")
  repo=$(_plugin_resolve_repo "$spec")
  dir="$(_plugin_dir)/$name"
  mkdir -p "$(_plugin_dir)"

  if [[ -x $dir/bin/$name && -f $dir/.plugin-meta ]]; then
    _plugin_path_add "$dir/bin"
    return 0
  fi

  installing=1

  if [[ -n $bpick ]]; then
    patterns=("$bpick")
  else
    patterns=($(_plugin_bpick_candidates)) ||
      { print -r "plugin: unsupported platform $(uname -s)/$(uname -m) for binary install" >&2; return 1 }
  fi

  url=$(_plugin_gh_asset_match "$repo" "${patterns[@]}") ||
    { print -r "plugin: no release asset for ${repo} on $(uname -s)/$(uname -m)" >&2; return 1 }

  print -r "plugin: installing binary ${name} for $(uname -s)/$(uname -m)..." >&2
  tmp=$(mktemp -d)
  local asset="${${url:t}%%\?*}"
  [[ -n $asset ]] || asset=asset
  command curl -fsSL "$url" -o "$tmp/$asset" ||
    { print -r "plugin: download failed for ${name}" >&2; rm -rf "$tmp"; (( installing )) && rm -rf "$dir"; return 1 }

  _plugin_extract_release "$tmp/$asset" "$tmp/root"
  content=$(_plugin_extract_root "$tmp/root")
  mkdir -p "$dir/bin"

  local bin dest="$dir/bin/$name"
  if [[ -f $content && ! -d $content ]]; then
    bin=$content
  elif [[ -n $pick ]]; then
    bin=$(_plugin_pick_binary "$content" "$pick") ||
      bin=$(_plugin_pick_binary "$content" "$name") ||
      bin=$(_plugin_pick_binary "$content" "*/$name") ||
      { print -r "plugin: no binary matches '${pick}' in ${name}" >&2; rm -rf "$tmp"; (( installing )) && rm -rf "$dir"; return 1 }
  elif [[ -f $content/extracted ]]; then
    bin="$content/extracted"
  else
    bin=$(_plugin_pick_binary "$content" "$name") ||
      bin=$(_plugin_pick_binary "$content" "*/$name") ||
      { print -r "plugin: could not locate binary for ${name}" >&2; rm -rf "$tmp"; (( installing )) && rm -rf "$dir"; return 1 }
  fi

  cp -f "$bin" "$dest"
  chmod +x "$dest"
  [[ -d $content/shell ]] && { rm -rf "$dir/shell"; cp -R "$content/shell" "$dir/shell" }
  if [[ $name == fzf && ! -d $dir/shell ]]; then
    _plugin_fetch_repo_subdir "$repo" shell "$dir/shell"
  fi
  rm -rf "$tmp"

  os=$(_plugin_host_os)
  arch=$(_plugin_host_arch)
  print -r "type=binary" > "$dir/.plugin-meta"
  print -r "repo=${repo}" >> "$dir/.plugin-meta"
  [[ -n $pick ]] && print -r "pick=${pick}" >> "$dir/.plugin-meta"
  print -r "bpick=${_PLUGIN_LAST_BPICK:-${patterns[1]}}" >> "$dir/.plugin-meta"
  [[ -n $os ]] && print -r "os=${os}" >> "$dir/.plugin-meta"
  [[ -n $arch ]] && print -r "arch=${arch}" >> "$dir/.plugin-meta"

  _plugin_path_add "$dir/bin"
}

plugin() {
  case ${1:-} in
    update)  shift; plugin_update "$@"; return ;;
    delete|rm) shift; plugin_delete "$@"; return ;;
    cleanup) shift; plugin_cleanup "$@"; return ;;
    register-paths) _plugin_register_binary_paths; return ;;
  esac

  local fpath_only=0 binary_only=0 spec subdir name url dir

  [[ $1 == --fpath ]] && { fpath_only=1; shift }
  [[ $1 == --binary || $1 == --bin ]] && { binary_only=1; shift }

  spec=$1
  subdir=${2:-}

  if [[ $spec == *://* ]]; then
    url=$spec
    name=${${spec%.git}:t}
  else
    url="https://github.com/${spec}.git"
    name=${spec:t}
  fi

  dir="$(_plugin_dir)/$name"
  mkdir -p "$(_plugin_dir)"

  if (( binary_only )); then
    plugin_binary "$spec" "$subdir" "${3:-}"
    return
  fi

  if [[ ! -d $dir ]]; then
    print -r "plugin: installing ${name}..." >&2
    git clone --depth=1 --quiet "$url" "$dir" \
      || { print -r "plugin: clone failed for ${name}" >&2; return 1 }
  fi

  if (( fpath_only )); then
    local target="${subdir:+$dir/$subdir}"
    target="${target:-$dir}"
    [[ -d $target ]] && fpath=("$target" $fpath)
    return
  fi

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

plugin_update() {
  local -a names
  local d name repo pick bpick

  if (( $# )); then
    names=("$@")
  else
    names=($(_plugin_dir)/*(N:t))
  fi

  for name in $names; do
    d="$(_plugin_dir)/$name"
    [[ -d $d ]] || { print -r "plugin: not found: ${name}" >&2; continue }

    if [[ -f $d/.plugin-meta ]]; then
      repo= pick= bpick=
      while IFS= read -r line; do
        case $line in
          repo=*)  repo=${line#repo=} ;;
          pick=*)  pick=${line#pick=} ;;
          bpick=*) bpick=${line#bpick=} ;;
        esac
      done < "$d/.plugin-meta"
      print -r "plugin: updating binary ${name}..." >&2
      rm -rf "$d"
      plugin_binary "${repo}" "${pick:-}" "${bpick:-}" || return 1
      continue
    fi

    [[ -d $d/.git ]] || { print -r "plugin: skip ${name} (not a git repo)" >&2; continue }
    print -r "plugin: updating ${name}..." >&2
    git -C "$d" pull --ff-only --quiet ||
      print -r "plugin: update failed for ${name}" >&2
  done
}

plugin_delete() {
  local name dir
  for name in "$@"; do
    dir="$(_plugin_dir)/$name"
    if [[ -d $dir ]]; then
      rm -rf "$dir"
      print -r "plugin: removed ${name}" >&2
    else
      print -r "plugin: not found: ${name}" >&2
    fi
  done
}

plugin_list_referenced() {
  local rc=$(_plugin_rc)
  [[ -f $rc ]] || return 1
  local line i spec
  local -a tokens
  while IFS= read -r line; do
    [[ $line == \#* || $line != plugin* ]] && continue
    tokens=(${(z)line})
    (( ${tokens[1]:-} == plugin )) || continue
    i=2
    while (( i <= ${#tokens} )); do
      case ${tokens[i]} in
        update|delete|rm|cleanup) break ;;
        --fpath|--binary|--bin) (( i++ )); continue ;;
        *)
          spec=${tokens[i]}
          _plugin_resolve_name "$spec"
          break
          ;;
      esac
      (( i++ ))
    done
  done < "$rc"
}

plugin_cleanup() {
  local dry_run=0
  [[ $1 == --dry-run ]] && { dry_run=1; shift }

  local -a referenced installed
  referenced=(${(uf)"$(plugin_list_referenced)"})
  installed=($(_plugin_dir)/*(N:t))

  local name removed=0
  for name in $installed; do
    if (( ${referenced[(Ie)$name]} )); then
      continue
    fi
    if (( dry_run )); then
      print -r "plugin: would remove ${name}" >&2
    else
      plugin_delete "$name"
    fi
    (( removed++ ))
  done

  if (( ! dry_run && ! removed )); then
    print -r "plugin: nothing to clean up" >&2
  fi
}

_plugin_register_binary_paths
