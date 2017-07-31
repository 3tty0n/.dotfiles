# load git settings
. $HOME/.config/fish/git.fish

set -x EDITOR "emacs"
set -x VISUAL "emacs"

# Aliases
alias l 'ls -1a'
alias vi 'vim'
alias e 'emacsclient -nw -a ""'
alias emacs 'e'
alias rm 'rm -i'
alias cp 'cp -i'
alias mv 'mv -i'
alias dc 'cd'
alias md 'mkdir '
alias pdfjoin '/System/Library/Automator/Combine\ PDF\ Pages.action/Contents/Resources/join.py'

if [ (uname) = "Darwin" ]
  set -x JAVA_HOME  (/usr/libexec/java_home -v 1.8)
end

# theme
set fish_theme bobthefish

switch (echo $fish_theme)
  case bobthefish
    set -g theme_display_date no
    set -g theme_color_scheme terminal
end

# opam settings
# see http://qiita.com/OKU_K/items/bb17e16474b391f5cc9d
if test -d ~/.opam
  source ~/.opam/opam-init/init.fish > /dev/null 2> /dev/null
end

# scalaenv
if test -d ~/.scalaenv
  set -gx PATH $HOME/.scalaenv/bin $PATH
  status --is-interactive; and source (scalaenv init -|psub)
end

# go
if test -d ~/.go
  set -x GOPATH (go env GOROOT)
  set -x GOPATH $HOME/.go
  set -x PATH $GOPATH/bin $GOROOT/bin $PATH
  set -U GHQ_SELECTOR fzf
end

if test -d ~/bin
  set -x fish_user_paths $fish_user_paths $HOME/bin
end

# functions
function docker-rm-all
  docker rm (docker ps -aq)
end

# fzf + emacs
function ef
  emacs -nw (fzf)
end

# fzf + vim
function vif
  vim (fzf)
end

# hide fish_greeging
function fish_greeting; end

# hide fish_right_prompt
function fish_right_prompt; end
