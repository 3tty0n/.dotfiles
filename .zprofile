# xwindow
if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
    exec startx
fi

##
# Your previous /Users/yusuke/.zprofile file was backed up as /Users/yusuke/.zprofile.macports-saved_2021-10-30_at_23:38:13
##

if [ -d "/opt/homebrew" ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
elif [ -d "~/.linuxbrew" ]; then
    eval "$(~/.linuxbrew/bin/brew shellenv)"
elif [ -d "/home/linuxbrew" ]; then
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

export QSYS_ROOTDIR="/home/yusuke/.cache/paru/clone/quartus-free/pkg/quartus-free-quartus/opt/intelFPGA/24.1/quartus/sopc_builder/bin"
