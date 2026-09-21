#!/usr/bin/env bash
# Show the usage of opencode-go (opencode zen).
#   For statusLine: no argument -> 1 line summary (go 5h 1% · wk 0% · mo 27%)
#   For /opencode-usage: --detail -> details of each slot
# Valid only in claude-go sessions.
set -uo pipefail

CACHE="${TMPDIR:-/tmp}/opencode-go-usage.json"
API="https://opencode.ai/zen/go/v1/usage"
DETAIL=0
[[ "${1:-}" == --detail ]] && DETAIL=1

if [[ "${ANTHROPIC_BASE_URL:-}" != *opencode* ]]; then
  (( DETAIL )) && echo "It is not claude-go session"
  exit 0
fi

mtime() { stat -c %Y "$1" 2>/dev/null || stat -f %m "$1" 2>/dev/null || echo 0; }

if (( $(date +%s) - $(mtime "$CACHE") >= 60 )); then
  curl -sf --max-time 10 -H "Authorization: Bearer ${OPENCODE_API_KEY:-}" \
    "$API" -o "$CACHE.tmp" && mv "$CACHE.tmp" "$CACHE"
fi
[[ -s "$CACHE" ]] || exit 0

if (( ! DETAIL )); then
  jq -r '.usage | "go 5h \(.rolling.percent)% · wk \(.weekly.percent)% · mo \(.monthly.percent)%"' "$CACHE"
  exit 0
fi

printf 'opencode-go usage\n'
jq -r --argjson now "$(date +%s)" \
  '.usage | to_entries[] | "\(.key)\t\(.value.percent)\t\(.value.status)\t\((.value.resetsAt | sub("\\.[0-9]+Z$"; "Z") | fromdateiso8601) - $now)"' \
  "$CACHE" |
  while IFS=$'\t' read -r w p s d; do
    (( d < 0 )) && d=0
    epoch=$(( $(date +%s) + d ))
    t=$(date -d "@$epoch" '+%m-%d %H:%M' 2>/dev/null || date -r "$epoch" '+%m-%d %H:%M')
    if (( d >= 86400 )); then left="$((d / 86400))d$((d % 86400 / 3600))h"; else left="$((d / 3600))h$((d % 3600 / 60))m"; fi
    printf '  %-7s %3d%%  %-3s  resets %s (%s 後)\n' "${w/rolling/5h}" "$p" "$s" "$t" "$left"
  done
