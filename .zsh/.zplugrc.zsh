zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-autosuggestions"
zplug "mollifier/cd-gitroot"
zplug "b4b4r07/enhancd", use:init.sh
zplug "junegunn/fzf-bin", as:command, from:gh-r, rename-to:fzf
zplug "peco/peco", as:command, from:gh-r, use:"*amd64*"
zplug "mrowa44/emojify", as:command
zplug "stedolan/jq", \
    from:gh-r, \
    as:command, \
    rename-to:jq
zplug "b4b4r07/emoji-cli", \
    on:"stedolan/jq"
zplug "b4b4r07/zsh-gomi", if:"which fzf"
zplug "changyuheng/zsh-interactive-cd"
zplug "tsub/4448666a276b088bce3f19005f512c15", from:gist # ghq-fzf.zsh
zplug "tsub/f4036e067a59b242a161fc3c8a5f01dd", from:gist # history-fzf.zsh
zplug "tsub/81ac9b881cf2475977c9cb619021ef3c", from:gist # ssh-fzf.zsh
zplug "tsub/29bebc4e1e82ad76504b1287b4afba7c", from:gist # tree-fzf.zsh

# oh-my-zsh
zplug "plugins/extract", from:oh-my-zsh

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

# Then, source plugins and add commands to $PATH
zplug load --verbose
