zplug "zplug/zplug", hook-build:'zplug --self-manage'

zplug "zsh-users/zsh-completions"

zplug "zsh-users/zsh-syntax-highlighting", defer:2

zplug "zsh-users/zsh-history-substring-search"

zplug "zsh-users/zsh-autosuggestions"

zplug "changyuheng/zsh-interactive-cd"

zplug "mollifier/cd-gitroot"

zplug "Tarrasch/zsh-bd", use:bd.zsh

zplug "b4b4r07/enhancd", use:init.sh

zplug "junegunn/fzf", \
      as:command, \
      use:bin/fzf-tmux

zplug "paulp/sbt-extras", \
      as:command, \
      use:sbt

zplug "k4rthik/git-cal", as:command

zplug "unixorn/rake-completion.zshplugin"

zplug "~/.zsh/util", from:local

zplug "~/.zsh/zsh-peco-history", from:local

zplug "~/.zsh/git-add-fzf", from:local

zplug "~/.zsh/ghq-fzf", from:local

zplug "modules/git", from:prezto

zplug "modules/prompt", from:prezto

zstyle ':prezto:module:prompt' theme 'pure'

if ! zplug check --verbose; then
  printf "Install? [y/N]: "
  if read -q; then
      echo; zplug install
  else
      echo
  fi
fi

zplug load
