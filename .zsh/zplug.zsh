zplug "zplug/zplug", hook-build:'zplug --self-manage'

zplug "zsh-users/zsh-completions", use:'src/_*', lazy:true

zplug "zdharma/fast-syntax-highlighting", defer:2
# zplug "zsh-users/zsh-syntax-highlighting", defer:2

zplug "zsh-users/zsh-history-substring-search"

zplug "zdharma/history-search-multi-word"

if zplug check "zsh-users/zsh-history-substring-search"; then
    bindkey -M emacs '^P' history-substring-search-up
    bindkey -M emacs '^N' history-substring-search-down
fi

zplug "zsh-users/zsh-autosuggestions"

zplug "mollifier/cd-gitroot"

# zplug "Tarrasch/zsh-bd", use:bd.zsh

# zplug "b4b4r07/enhancd", use:init.sh

zplug "rupa/z", use:z.sh

zplug "supercrabtree/k"

zplug "tj/git-extras", as:command, use:'bin/git-*'

zplug "motemen/ghq", as:command, from:gh-r, rename-to:ghq

zplug "jhawthorn/fzy", as:command, use:'./fzy', hook-build:'make'

zplug "junegunn/fzf-bin", from:gh-r, as:command, rename-to:fzf

zplug "k4rthik/git-cal", as:command

zplug "peco/peco", as:command, from:gh-r, use:"*amd64*"

zplug "hlissner/zsh-autopair", defer:2

zplug "~/.zsh/util", from:local

###################
##### theme #######
###################

# zplug "bhilburn/powerlevel9k", use:powerlevel9k.zsh-theme, as:theme

if zplug check "bhilburn/powerlevel9k"; then
    # powerlevel9k setting
    POWERLEVEL9K_MODE='nerdfont-complete'
    POWERLEVEL9K_SHORTEN_DIR_LENGTH=1
    POWERLEVEL9K_SHORTEN_DELIMITER=""
    POWERLEVEL9K_SHORTEN_STRATEGY="truncate_from_right"

    # prompt
    POWERLEVEL9K_PROMPT_ON_NEWLINE=true
    POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX=""
    POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX="λ "

    POWERLEVEL9K_CUSTOM_WIFI_SIGNAL="zsh_wifi_signal"
    POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(os_icon status battery dir vcs)
    POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(pyenv rbenv)
fi

zplug "denysdovhan/spaceship-prompt", use:spaceship.zsh, as:theme

# zplug "mafredri/zsh-async"
# zplug "sindresorhus/pure", from:github, use:pure.zsh, as:theme

if [ -x "$(which kubectl)" ]; then
    source <(kubectl completion zsh)
fi

if zplug check "jonmosco/kube-ps1"; then
    PROMPT='$(kube_ps1) '$PROMPT
    KUBE_PS1_BINARY=oc
fi

if ! zplug check --verbose; then
  printf "Install? [y/N]: "
  if read -q; then
    echo; zplug install
  else
    echo
  fi
fi

zplug load
