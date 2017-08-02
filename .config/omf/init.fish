# Aliases
alias g 'git'
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

set -x EDITOR "emacs"
set -x VISUAL "emacs"

# opam settings
# see http://qiita.com/OKU_K/items/bb17e16474b391f5cc9d
if test -d ~/.opam
  source ~/.opam/opam-init/init.fish > /dev/null 2> /dev/null
end

# java
if [ (uname) = "Darwin" ]
  set -x JAVA_HOME  (/usr/libexec/java_home -v 1.8)
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

# Add bin to PATH
if test -d ~/bin
  set -x PATH $HOME/bin $PATH
end

set -x PATH /usr/local/share/git-core/contrib/diff-highlight $PATH
set -x FZF_DEFAULT_OPTS '--height 40% --reverse --border'

# hide fish_greeging
function fish_greeting; end

# hide fish_right_prompt
function fish_right_prompt; end
