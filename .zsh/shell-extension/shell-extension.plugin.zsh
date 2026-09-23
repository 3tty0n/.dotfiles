# shell_extension: guarded loading for shell settings.
#
# Two helpers that skip whatever is absent, so a tool or file missing on this
# machine is a no-op rather than an error:
#
#   shell_extension_source <file>...       source each readable file
#   shell_extension_eval <cmd> [args...]   eval "$(cmd args)" if cmd is on PATH
#
# Each returns true only if it loaded something, so
# `shell_extension_source a || shell_extension_source b` is a fallback chain.

shell_extension_source() {
  # NB: not `path` — that name is tied to $PATH, and shadowing it locally
  # breaks command lookup for everything sourced in here.
  local fragment found=0
  for fragment in "$@"; do
    [[ -r $fragment ]] && { source "$fragment"; found=1 }
  done
  (( found ))
}

shell_extension_eval() {
  local cmd=$1
  shift
  command -v "$cmd" >/dev/null && eval "$("$cmd" "$@")"
}
