zplug "zplug/zplug", hook-build:'zplug --self-manage'

zplug "zsh-users/zsh-completions"

zplug "zsh-users/zsh-syntax-highlighting", defer:2

zplug "zsh-users/zsh-history-substring-search"

zplug "zsh-users/zsh-autosuggestions"

zplug "changyuheng/zsh-interactive-cd"

zplug "mollifier/cd-gitroot"

zplug "Tarrasch/zsh-bd", use:bd.zsh

zplug "b4b4r07/enhancd", use:init.sh

# zplug "rupa/z", use:z.sh

zplug "supercrabtree/k"

zplug "junegunn/fzf", \
      as:command, \
      use:fzf, \
      hook-build:'go get -d && go build'

zplug "junegunn/fzf", \
      as:command, \
      use:bin/fzf-tmux, \
      frozen:1

zplug "motemen/ghq", \
      as:command, \
      use:ghq, \
      hook-build:'make build'

zplug "paulp/sbt-extras", \
      as:command, \
      use:sbt

zplug "k4rthik/git-cal", as:command

zplug "~/.zsh/util", from:local

# zplug "bhilburn/powerlevel9k", \
#       use:powerlevel9k.zsh-theme, \
#       as:theme

zplug denysdovhan/spaceship-prompt, \
      use:spaceship.zsh, \
      from:github, \
      as:theme

if ! zplug check --verbose; then
  printf "Install? [y/N]: "
  if read -q; then
    echo; zplug install
  else
    echo
  fi
fi

zplug load
