ARCH=$(uname -m)

if [[ $ARCH == arm64 ]]; then
    eval $(/opt/homebrew/bin/brew shellenv)
elif [[ $ARCH == x86_64 ]]; then
    eval $(/usr/local/bin/brew shellenv)
fi

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
