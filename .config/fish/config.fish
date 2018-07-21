# Path to Oh My Fish install.
set -q XDG_DATA_HOME
  and set -gx OMF_PATH "$XDG_DATA_HOME/omf"
  or set -gx OMF_PATH "$HOME/.local/share/omf"

set -gx OMF_CONFIG "$HOME/.config/omf"

# rbenv
set -gx RBENV_ROOT $HOME/.rbenv

# pyenv
set -gx PYENV_ROOT $HOME/.pyenv

# java
set -gx JAVA_HOME (/usr/libexec/java_home -v 1.8)

# scalaenv
# set -gx PATH $HOME/.scalaenv/bin $PATH
# status --is-interactive; and source (scalaenv init -|psub)

# go
set -gx GOROOT (go env GOROOT)
set -gx GOPATH $HOME/.go
set -gx PATH $GOPATH/bin $GOROOT/bin $PATH
set -gx GHQ_SELECTOR fzf

# git-diff
set -x PATH /usr/local/share/git-core/contrib/diff-highlight $PATH

# fzf
set -x FZF_DEFAULT_OPTS '--height 40% --reverse --border'

# editor
set -Ux EDITOR 'emacsclient -n'

alias g 'git'
alias l 'ls -1a'
alias vi 'vim'
alias e 'emacsclient -nw -a ""'
alias rm 'rm -i'
alias cp 'cp -i'
alias mv 'mv -i'
alias dc 'cd'
alias md 'mkdir '
alias pdfjoin '/System/Library/Automator/Combine\ PDF\ Pages.action/Contents/Resources/join.py'

eval (hub alias -s)

# hide fish_greeging
function fish_greeting; end

# hide fish_right_prompt
function fish_right_prompt; end
