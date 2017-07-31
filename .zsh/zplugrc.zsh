zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-autosuggestions"
zplug "b4b4r07/enhancd", use:init.sh
zplug "junegunn/fzf-bin", as:command, from:gh-r, rename-to:fzf
zplug "peco/peco", as:command, from:gh-r, use:"*amd64*"
zplug "changyuheng/zsh-interactive-cd"

# gist
zplug "tsub/f4036e067a59b242a161fc3c8a5f01dd", from:gist # history-fzf.zsh
zplug "tsub/81ac9b881cf2475977c9cb619021ef3c", from:gist # ssh-fzf.zsh
zplug "tsub/29bebc4e1e82ad76504b1287b4afba7c", from:gist # tree-fzf.zsh
zplug "3tty0n/259e8d65b0e7fd375d6c9545d67e85f0", from:gist # fzf-git-log.zsh
zplug "3tty0n/fbfa5b88feed5bc5d3b5f5e59d80a2d8", from:gist # fzf-git-branch.zsh
zplug "3tty0n/b5a035e611357e1805b690c7f0e4b1c2", from:gist # fzf-git-add.zsh
zplug "3tty0n/1a5ed257ca3cdda411386c2cbd3b1acc", from:gist # fzf-git-stash.zsh
zplug "3tty0n/0c7287deedd0bedc1bc0dc23b51653aa", from:gist # ghq-fzf.zsh
zplug "3tty0n/4699f9ada7a8eb9fac7e15bebfb65c8d", from:gist # delete-gomi.zsh

# local settings
zplug "~/.zsh/completion", from:local
zplug "~/.zsh", from:local, use:powerline.zsh

# oh-my-zsh
# zplug "plugins/extract", from:oh-my-zsh

#if ! zplug check --verbose; then
#  printf "Install? [y/N]: "
#  if read -q; then
#      echo; zplug install
#  fi
#fi

zplug load --verbose
