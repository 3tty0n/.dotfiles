#!/bin/sh
input=$(cat)
cwd=$(echo "$input" | jq -r '.workspace.current_dir // .cwd // "."')
model=$(echo "$input" | jq -r '.model.display_name // ""')
used=$(echo "$input" | jq -r '.context_window.used_percentage // empty')

# Shorten the directory path like zsh's _shorten_dir:
# replace $HOME prefix with ~, then abbreviate intermediate segments to first char
home="$HOME"
short_cwd="${cwd#"$home"}"
if [ "$short_cwd" != "$cwd" ]; then
    short_cwd="~${short_cwd}"
fi

# Abbreviate intermediate path segments (all but last) to first character
base="${short_cwd##*/}"
dir="${short_cwd%/*}"
if [ "$dir" != "$short_cwd" ] && [ -n "$dir" ] && [ "$dir" != "~" ] && [ "$dir" != "" ]; then
    abbreviated=$(printf '%s' "$dir" | sed 's|/\([^/]\)[^/]*|/\1|g')
    short_cwd="${abbreviated}/${base}"
fi

# Git branch from the workspace git worktree or via git command
branch=$(echo "$input" | jq -r '.workspace.git_worktree // empty')
if [ -z "$branch" ]; then
    branch=$(git -C "$cwd" --no-optional-locks symbolic-ref --short HEAD 2>/dev/null || \
             git -C "$cwd" --no-optional-locks rev-parse --short HEAD 2>/dev/null)
fi

# SSH indicator (orange #e0af68), matching zsh's _prompt_ssh
if [ -n "$SSH_CONNECTION" ] || [ -n "$SSH_TTY" ]; then
    ssh_part=$(printf '\033[38;2;224;175;104m@%s\033[0m ' "$(hostname -s)")
else
    ssh_part=""
fi

# Tokyo Night colors: blue #7aa2f7, purple #bb9af7, green #9ece6a
dir_part=$(printf '\033[38;2;122;162;247m%s\033[0m' "$short_cwd")

if [ -n "$branch" ]; then
    branch_part=$(printf ' \033[38;2;187;154;247m%s\033[0m' "$branch")
else
    branch_part=""
fi

if [ -n "$model" ]; then
    model_part=$(printf ' \033[38;2;158;206;106m[%s]\033[0m' "$model")
else
    model_part=""
fi

if [ -n "$used" ]; then
    ctx_part=$(printf ' \033[38;2;158;206;106m[ctx:%.0f%%]\033[0m' "$used")
else
    ctx_part=""
fi

printf '%s%s%s%s%s' "$ssh_part" "$dir_part" "$branch_part" "$model_part" "$ctx_part"
