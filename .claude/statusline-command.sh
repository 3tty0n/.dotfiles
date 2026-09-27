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
effort=$(echo "$input" | jq -r '.effort.level')
if [ -n "$model" ]; then
  output="${output}$(printf "  ${DIM}${CYAN}%s (%s)${RESET}" "$model" "$effort")"
fi

used=$(echo "$input" | jq -r '.context_window.used_percentage // 0')
if [ -n "$used" ]; then
  used_int=$(printf '%.0f' "$used")
  output="${output}$(printf "  ${DIM}${YELLOW}%s%%${RESET}" "$used_int") context"
fi

# --- context remaining % ---
#remaining=$(echo "$input" | jq -r '.context_window.remaining_percentage // empty')
#if [ -n "$remaining" ]; then
#  remaining_int=$(printf '%.0f' "$remaining")
#  output="${output}$(printf "  ${DIM}${YELLOW}ctx:%s%%%${RESET}" "$remaining_int")"
#fi

# --- usage vs limit (5h / 7d windows, Pro/Max only) ---
# These fields are absent on plans/sessions where Claude Code does not expose rate limits.
usage_5h=$(printf '%s' "$input" | jq -r '.rate_limits.five_hour.used_percentage // empty' 2>/dev/null)
usage_7d=$(printf '%s' "$input" | jq -r '.rate_limits.seven_day.used_percentage // empty' 2>/dev/null)
reset_5h=$(printf '%s' "$input" | jq -r '.rate_limits.five_hour.resets_at // empty' 2>/dev/null)
reset_7d=$(printf '%s' "$input" | jq -r '.rate_limits.seven_day.resets_at // empty' 2>/dev/null)

format_remaining() {
  reset_at="$1"
  [ -z "$reset_at" ] && return

  case "$reset_at" in
    *[!0-9.]*) reset_epoch=$(date -d "$reset_at" +%s 2>/dev/null) || return ;;
    *) reset_epoch=${reset_at%.*} ;;
  esac
  now_epoch=$(date +%s)
  diff=$((reset_epoch - now_epoch))
  [ "$diff" -lt 0 ] && diff=0

  days=$((diff / 86400))
  hours=$(((diff % 86400) / 3600))
  minutes=$(((diff % 3600) / 60))

  if [ "$days" -gt 0 ]; then
    printf '%dd%dh%02dm @%s' "$days" "$hours" "$minutes" "$(date -d "@$reset_epoch" '+%m/%d %H:%M')"
  elif [ "$hours" -gt 0 ]; then
    printf '%dh%02dm @%s' "$hours" "$minutes" "$(date -d "@$reset_epoch" '+%H:%M')"
  else
    printf '%dm @%s' "$minutes" "$(date -d "@$reset_epoch" '+%H:%M')"
  fi
}

remaining_5h=$(format_remaining "$reset_5h")
remaining_7d=$(format_remaining "$reset_7d")

if [ -n "$usage_5h" ]; then
  output="${output}$(printf "  ${DIM}${PURPLE}5h:%s%%%s${RESET}" \
    "$(printf '%.0f' "$usage_5h")" \
    "$([ -n "$remaining_5h" ] && printf ' (%s)' "$remaining_5h")")"
fi

if [ -n "$usage_7d" ]; then
  output="${output}$(printf "  ${DIM}${PURPLE}7d:%s%%%s${RESET}" \
    "$(printf '%.0f' "$usage_7d")" \
    "$([ -n "$remaining_7d" ] && printf ' (%s)' "$remaining_7d")")"
fi

printf '%s' "$output"
