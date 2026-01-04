#
# ~/.bashrc
#
if [ -f ~/.bash/git-completion.bash ]; then
    . ~/.bash/git-completion.bash
fi

if [ -f ~/.bash/git-prompt.sh ]; then
    . ~/.bash/git-prompt.sh
    export GIT_PS1_SHOWDIRTYSTATE=true
    export GIT_PS1_SHOWUNTRACKEDFILES=true
    export GIT_PS1_SHOWUPSTREAM="auto"
    export GIT_PS1_STATESEPARATOR=" "
fi

function set_bash_prompt() {
    local EXIT="$?" # 直前の終了ステータスを保持

    local RS='\[\033[0m\]'    # Reset
    local G='\[\033[32m\]'    # Green
    local R='\[\033[31m\]'    # Red
    local B='\[\033[34m\]'    # Blue
    local C='\[\033[36m\]'    # Cyan

    local STATUS_COLOR=$G
    if [ $EXIT -ne 0 ]; then
        STATUS_COLOR=$R
    fi

    PS1="${STATUS_COLOR}[\u@\h ${B}\w${STATUS_COLOR}]${C}$(__git_ps1 ' (git:%s)')${RS}\n\$ "
}

PROMPT_COMMAND=set_bash_prompt

if [ -f ~/.bash/z.sh ]; then
    . ~/.bash/z.sh
fi

fz() {
    local dir
    SELECTOR=fzf
    if [ ! -x "$(which fzf)" ]; then
        echo "Should install fzf"
        exit 1
    fi
    dir=$(z -l 2>&1 | fzf --height 40% --nth 2.. --reverse --inline-info +s --tac --query "$*" | awk '{ print $2 }')

    if [ -n "$dir" ]; then
        cd "$dir"
    fi
}
bind -x '"\C-g": fz'

alias g='git'
alias ls='ls --color'
alias l='ls --color -1a'
alias ll='ls --color -la'

# >>> juliaup initialize >>>

# !! Contents within this block are managed by juliaup !!

case ":$PATH:" in
    *:/home/yusuke/.juliaup/bin:*)
        ;;

    *)
        export PATH=/home/yusuke/.juliaup/bin${PATH:+:${PATH}}
        ;;
esac

# <<< juliaup initialize <<<
