#!/usr/bin/env bash
# fzf preview helper for ghq repositories (ghq list lines → repo path under ghq root).

set -euo pipefail

repo="${1:-}"
[[ -z $repo ]] && exit 0

root="$(ghq root 2>/dev/null)" || exit 0
dir="${root}/${repo}"
[[ -d $dir ]] || exit 0

if git -C "$dir" rev-parse --git-dir &>/dev/null; then
  git -C "$dir" log --oneline --decorate -5 2>/dev/null
  echo
  git -C "$dir" status -sb 2>/dev/null
  echo
fi

if command -v eza &>/dev/null; then
  eza -l -g -a --icons "$dir" 2>/dev/null
else
  ls -la "$dir" 2>/dev/null
fi
