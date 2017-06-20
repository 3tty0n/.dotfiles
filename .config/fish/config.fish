# load git settings
. $HOME/.config/fish/git.fish

set -x EDITOR "emacs"
set -x VISUAL "emacs"

# Aliases
alias l 'ls -la'
alias vi 'vim'
alias e 'emacs -nw'
alias rm 'rm -i'
alias cp 'cp -i'
alias mv 'mv -i'
alias dc 'cd'
alias md 'mkdir '
alias pdfjoin '/System/Library/Automator/Combine\ PDF\ Pages.action/Contents/Resources/join.py'

if [ (uname) = "Darwin" ]
  set -x JAVA_HOME  (/usr/libexec/java_home -v 1.8)
end

set -U fish_user_paths $fish_user_paths $HOME/bin

# theme
set fish_theme bobthefish

switch (echo $fish_theme)
  case bobthefish
    set -g theme_display_date no
    set -g theme_color_scheme solarized #terminal
end

function fish_greeting; end
function fish_right_prompt; end

# cd > ls
# function cd;  builtin cd $argv;  ls -la;  end

# opam settings
if test -d ~/.opam
  source ~/.opam/opam-init/init.fish > /dev/null 2> /dev/null
end

if test -d ~/.scalaenv
  set -x PATH $HOME/.scalaenv $PATH
end

if test -d ~/bin
  set -x PATH $HOME/bin $PATH
end

function docker-rm-all
  docker rm (docker ps -aq)
end

test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish
