zplug "zplug/zplug", hook-build:'zplug --self-manage'

zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-syntax-highlighting", defer:2
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-autosuggestions"
zplug "changyuheng/zsh-interactive-cd"
zplug "mollifier/cd-gitroot"
zplug "~/.zsh/functions", from:local

zplug "clvv/fasd", \
      as:command, \
      use:fasd

zplug "b4b4r07/enhancd", \
      use:init.sh

zplug "~/.zsh/util", \
      from:local

zplug "motemen/ghq", \
    as:command, \
    from:gh-r, \
    rename-to:ghq


# gist
zplug "tsub/f4036e067a59b242a161fc3c8a5f01dd", from:gist # history-fzf.zsh
zplug "3tty0n/0c7287deedd0bedc1bc0dc23b51653aa", from:gist # ghq-fzf.zsh

zplug load
