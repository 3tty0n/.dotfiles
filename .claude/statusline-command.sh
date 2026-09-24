#!/bin/sh
# Claude Code statusLine — mirrors the zsh PROMPT style:
#   shortened_dir [branch]  model  ctx%
# Colors mirror the Tokyo Night palette used in the zsh PROMPT.

input=$(cat)
cwd=$(echo "$input" | jq -r '.cwd // .workspace.current_dir // empty')
[ -z "$cwd" ] && cwd=$(pwd)

# Shorten directory: collapse intermediate components to first letter, keep last
shorten_dir() {
  home_dir="${HOME:-/root}"
  p="$1"
  # Replace $HOME prefix with ~
  case "$p" in
    "$home_dir"*) p="~${p#$home_dir}" ;;
  esac
  # If root, ~ or single component → return as-is
  case "$p" in
    / | "~" ) printf '%s' "$p"; return ;;
  esac
  base="${p##*/}"
  dir="${p%/*}"
  if [ "$dir" = "" ] || [ "$dir" = "~" ] || [ "$dir" = "$base" ]; then
    printf '%s' "$p"
    return
  fi
  # Shorten each intermediate path component to its first character
  shortened=$(printf '%s' "$dir" | sed 's|/\([^/]\)[^/]*/|/\1/|g; s|/\([^/]\)[^/]*$|/\1|')
  printf '%s/%s' "$shortened" "$base"
}

# Git branch (no optional lock to avoid contention)
git_branch() {
  git -C "$1" --no-optional-locks symbolic-ref --short HEAD 2>/dev/null \
    || git -C "$1" --no-optional-locks rev-parse --short HEAD 2>/dev/null
}

# ANSI colors (Tokyo Night palette)
BLUE='\033[38;2;122;162;247m'    # #7aa2f7  dir
PURPLE='\033[38;2;187;154;247m'  # #bb9af7  branch
CYAN='\033[38;2;125;207;255m'    # #7dcfff  model
YELLOW='\033[38;2;224;175;104m'  # #e0af68  context
DIM='\033[2m'
RESET='\033[0m'

# --- dir + branch ---
short_dir=$(shorten_dir "$cwd")
branch=$(git_branch "$cwd" 2>/dev/null)

output=$(printf "${BLUE}%s${RESET}" "$short_dir")
[ -n "$branch" ] && output="${output}$(printf " ${PURPLE}%s${RESET}" "$branch")"

# --- model (short name) ---
model=$(echo "$input" | jq -r '.model.display_name // empty')
if [ -n "$model" ]; then
  output="${output}$(printf "  ${DIM}${CYAN}%s${RESET}" "$model")"
fi

# --- context remaining % ---
remaining=$(echo "$input" | jq -r '.context_window.remaining_percentage // empty')
if [ -n "$remaining" ]; then
  remaining_int=$(printf '%.0f' "$remaining")
  output="${output}$(printf "  ${DIM}${YELLOW}ctx:%s%%%${RESET}" "$remaining_int")"
fi

# --- usage vs limit (5h / 7d windows, Pro/Max only) ---
# Team plans have no seven_day window; each part is skipped when absent
limits=$(echo "$input" | jq -r '
  [(.rate_limits.five_hour.used_percentage? // empty | "5h:\(round)%"),
   (.rate_limits.seven_day.used_percentage? // empty | "7d:\(round)%")]
  | join(" ")' 2>/dev/null)
if [ -n "$limits" ]; then
  output="${output}$(printf "  ${DIM}${PURPLE}%s${RESET}" "$limits")"
fi

printf '%s' "$output"
