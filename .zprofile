[[ ! -f ~/.profile ]] || source ~/.profile

# direnv
if [ -x "`which direnv`" ]; then
    eval "$(direnv hook zsh)"
fi

# opam
if [ -x "`which opam`" ]; then
    eval $(opam config env)
fi

# xwindow
if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
    exec startx
fi

##
# Your previous /Users/yusuke/.zprofile file was backed up as /Users/yusuke/.zprofile.macports-saved_2021-10-30_at_23:38:13
##

# MacPorts Installer addition on 2021-10-30_at_23:38:13: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

