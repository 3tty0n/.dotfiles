# zprezto
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# zplug
if [[ -e ~/.zplug/init.zsh ]]; then
  source ~/.zplug/init.zsh
  source ~/.zplugrc.zsh
else
  curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh| zsh
  sleep 10
fi

# internal settings
setopt auto_menu
setopt auto_cd
setopt auto_list
setopt auto_param_keys
setopt auto_param_slash
setopt auto_param_slash
setopt list_packed
setopt rec_exact
setopt correct
setopt globdots
setopt interactive_comments
setopt no_beep
setopt nolistbeep
setopt no_tify
setopt list_types

alias vi='vim'
alias e='emacs -nw'
alias dc=cd
alias rm='rm -ri'
alias cdu='cd-gitroot'

vif () { vim $(fzf) }
ef () { emacs -nw $(fzf) }

# path
# pyenv
if [ -e ~/.pyenv ]; then
  export PYENV_ROOT=$HOME/.pyenv
  export PATH="$PYENV_ROOT/shims:$PATH"
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi

# rbenv
if [ -e ~/.rbenv ]; then
  export RBENV_ROOT=$HOME/.rbenv
  eval "$(rbenv init -)"
fi

# java
export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
export PATH="$JAVA_HOME:$PATH"

# scalaenv
if [ -e ~/.scalaenv ]; then
  export PATH="${HOME}/.scalaenv/bin:${PATH}"
  eval "$(scalaenv init -)"
fi

# go
if [ -x "`which go`" ]; then
  export GOROOT=/usr/local/opt/go/libexec
  export GOPATH=$HOME/.go
  export PATH=$PATH:$GOPATH/bin
fi

# manual bin
if [ -e ~/bin ]; then
  export PATH="${HOME}/bin:${PATH}"
fi

# OPAM configuration
[ -e ~/.opam ] && source ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

#
# enhancd settings
#

# `ls` after `cd` in enhancd
ENHANCD_HOOK_AFTER_CD=l

#
# custom settings
#

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# incr
# [ -f ~/.zsh/incr/incr.zsh ] &&  source ~/.zsh/incr/incr.zsh

# npm completion
[ -f ~/.zsh/completion/npm.zsh ] && source ~/.zsh/completion/npm.zsh

#
# fzf settings
#

# fshow - git commit browser
fshow() {
  git log --graph --color=always \
      --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
  fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
      --bind "ctrl-m:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                {}
FZF-EOF"
}

# fbr - checkout git branch (including remote branches), sorted by most recent commit, limit 30 last branches
fbr() {
  local branches branch
  branches=$(git for-each-ref --count=30 --sort=-committerdate refs/heads/ --format="%(refname:short)") &&
  branch=$(echo "$branches" |
           fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
  git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}

fadd() {
  local out q n addfiles
  while out=$(
      git status --short |
      awk '{if (substr($0,2,1) !~ / /) print $2}' |
      fzf-tmux --multi --exit-0 --expect=ctrl-d); do
    q=$(head -1 <<< "$out")
    n=$[$(wc -l <<< "$out") - 1]
    addfiles=(`echo $(tail "-$n" <<< "$out")`)
    [[ -z "$addfiles" ]] && continue
    if [ "$q" = ctrl-d ]; then
      git diff --color=always $addfiles | less -R
    else
      git add $addfiles
    fi
  done
}

# fstash - easier way to deal with stashes
# type fstash to get a list of your stashes
# enter shows you the contents of the stash
# ctrl-d shows a diff of the stash against your current HEAD
# ctrl-b checks the stash out as a branch, for easier merging
fstash() {
  local out q k sha
  while out=$(
    git stash list --pretty="%C(yellow)%h %>(14)%Cgreen%cr %C(blue)%gs" |
    fzf --ansi --no-sort --query="$q" --print-query \
        --expect=ctrl-d,ctrl-b);
  do
    mapfile -t out <<< "$out"
    q="${out[0]}"
    k="${out[1]}"
    sha="${out[-1]}"
    sha="${sha%% *}"
    [[ -z "$sha" ]] && continue
    if [[ "$k" == 'ctrl-d' ]]; then
      git diff $sha
    elif [[ "$k" == 'ctrl-b' ]]; then
      git stash branch "stash-$sha" $sha
      break;
    else
      git stash show -p $sha
    fi
  done
}

#
# zsh-history-substring-search settings
#

bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down


# docker completion
zstyle ':completion:*:*:docker:*' option-stacking yes
zstyle ':completion:*:*:docker-*:*' option-stacking yes

delete_dotfiles () {
  find $1 \
    \( -name '.DS_Store' \
    -o -name '._*' \
    -o -name '.apdisk' \
    -o -name 'Thumbs.db' \
    -o -name 'Desktop.ini' \
    \) -delete -print;
}

dtask () { date +'%Y%m%d' }

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

if [ ~/.zshrc -nt ~/.zshrc.zwc ]; then
   zcompile ~/.zshrc
fi

if [ ~/.zplugrc.zsh -nt ~/.zplugrc.zsh.zwc ]; then
  zcompile ~/.zplugrc.zsh
fi
