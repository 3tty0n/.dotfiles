zplug "zplug/zplug", \
      hook-build:'zplug --self-manage'

zplug "zsh-users/zsh-completions"

zplug "zsh-users/zsh-syntax-highlighting", \
      defer:2

zplug "zsh-users/zsh-history-substring-search"

zplug "zsh-users/zsh-autosuggestions"

zplug "changyuheng/zsh-interactive-cd"

zplug "mollifier/cd-gitroot"

zplug "Tarrasch/zsh-bd", \
      use:bd.zsh

zplug "b4b4r07/enhancd", \
      use:init.sh

zplug "motemen/ghq", \
      as:command, \
      from:gh-r, \
      rename-to:ghq

zplug "junegunn/fzf", \
      as:command, \
      use:bin/fzf-tmux

zplug "~/.zsh/util", \
      from:local

zplug "~/.zsh/zsh-peco-history", \
      from:local

zplug "~/.zsh/ghq-fzf", \
      from:local

zplug "bhilburn/powerlevel9k",\
      use:powerlevel9k.zsh-theme

zplug "paulp/sbt-extras", \
      as:command, \
      use:sbt

#if ! zplug check; then
#  zplug install
#fi

zplug load
