zplug "zplug/zplug", hook-build:'zplug --self-manage'

zplug "zsh-users/zsh-completions"

# zplug "zsh-users/zsh-syntax-highlighting", defer:2
zplug "zdharma/fast-syntax-highlighting", defer:2

zplug "zsh-users/zsh-history-substring-search"

if zplug check "zsh-users/zsh-history-substring-search"; then
    bindkey -M emacs '^P' history-substring-search-up
    bindkey -M emacs '^N' history-substring-search-down
fi

zplug "zsh-users/zsh-autosuggestions"

zplug "mollifier/cd-gitroot"

zplug "Tarrasch/zsh-bd", use:bd.zsh

zplug "b4b4r07/enhancd", use:init.sh

zplug "rupa/z", use:z.sh

zplug "andrewferrier/fzf-z"

zplug "supercrabtree/k"

zplug "tj/git-extras", as:command, use:'bin/git-*'

case "${OSTYPE}" in
    darwin* )
        zplug "junegunn/fzf-bin", \
              as:command, \
              from:gh-r, \
              rename-to:"fzf", \
              use:"*darwin*amd64*"

        zplug "peco/peco", \
              as:command, \
              from:gh-r, \
              use:"*amd64*"
	;;
    linux* )
	if [ $(uname -m) = 'i686' ]; then
	    zplug "junegunn/fzf-bin", \
		  as:command, \
		  from:gh-r, \
		  rename-to:"fzf", \
		  use:"*linux*386*"

	    zplug "peco/peco", \
		  as:command, \
		  from:gh-r, \
		  use:"*linux*386*"
	else
	    zplug "junegunn/fzf-bin", \
		  as:command, \
		  from:gh-r, \
		  rename-to:"fzf", \
		  use:"*linux*amd64*"

	    zplug "peco/peco", \
		  as:command, \
		  from:gh-r, \
		  use:"*linux*amd64*"
	fi
	;;
esac

zplug "junegunn/fzf", as:command, use:"bin/fzf-tmux"

if zplug check "junegunn/fzf-bin"; then
  export FZF_DEFAULT_OPTS="--height 40% --reverse --border"
fi

zplug "motemen/ghq",\
      as:command, \
      from:gh-r, \
      rename-to:ghq


zplug "paulp/sbt-extras", as:command, use:sbt

zplug "k4rthik/git-cal", as:command

zplug "jonmosco/kube-ps1", \
      use:kube-ps1.sh, \
      as:theme

zplug "~/.zsh/util", from:local

###################
##### theme #######
###################

# zplug "bhilburn/powerlevel9k", use:powerlevel9k.zsh-theme, as:theme

if zplug check "bhilburn/powerlevel9k"; then
    # powerlevel9k setting
    POWERLEVEL9K_SHORTEN_DIR_LENGTH=1
    POWERLEVEL9K_SHORTEN_DELIMITER=""
    POWERLEVEL9K_SHORTEN_STRATEGY="truncate_from_right"

    # prompt
    POWERLEVEL9K_PROMPT_ON_NEWLINE=true
    POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX=""
    POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX="λ "

    POWERLEVEL9K_CUSTOM_WIFI_SIGNAL="zsh_wifi_signal"
    POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(status time dir vcs)
    POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=()
fi

zplug "denysdovhan/spaceship-prompt", use:spaceship.zsh, as:theme

# zplug "mafredri/zsh-async"
# zplug "sindresorhus/pure", from:github, use:pure.zsh, as:theme

if ! zplug check --verbose; then
  printf "Install? [y/N]: "
  if read -q; then
    echo; zplug install
  else
    echo
  fi
fi

zplug load
