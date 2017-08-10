zplug "zplug/zplug", hook-build:'zplug --self-manage'

zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-syntax-highlighting", defer:2
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-autosuggestions"
zplug "changyuheng/zsh-interactive-cd"
zplug "mollifier/cd-gitroot"
zplug "~/.zsh/functions", from:local

zplug "rupa/z", use:z.sh

zplug "b4b4r07/enhancd", \
      use:init.sh

zplug "~/.zsh/util", \
      from:local

zplug "motemen/ghq", \
    as:command, \
    from:gh-r, \
    rename-to:ghq

zplug "junegunn/fzf-bin", \
      from:gh-r, \
      as:command,\
      rename-to:fzf

zplug "peco/peco", \
      as:command, \
      from:gh-r

zplug load
