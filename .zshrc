## Added by Zinit's installer
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

bindkey -e

zinit load zdharma-continuum/history-search-multi-word

# Directory listings for zsh with git features
zinit light supercrabtree/k

zinit ice from"gh-r" as"program" pick"*/ghq"
zinit light "x-motemen/ghq"

zinit pack for fzf

zinit light changyuheng/fz

zinit light rupa/z

zinit light zsh-users/zsh-autosuggestions

zinit light zdharma-continuum/fast-syntax-highlighting

zinit light zsh-users/zsh-history-substring-search

zinit ice blockf atpull'zinit creinstall -q .'
zinit light zsh-users/zsh-completions

zinit light hlissner/zsh-autopair

#zinit ice src"auto-notify.plugin.zsh"; zinit light MichaelAquilina/zsh-auto-notify

zinit ice as"program" pick"$ZPFX/bin/git-*" make"PREFIX=$ZPFX"; zinit light tj/git-extras

# Load starship theme
zinit ice as"command" from"gh-r" \
          atclone"./starship init zsh > init.zsh; ./starship completions zsh > _starship" \
          atpull"%atclone" src"init.zsh" # pull behavior same as clone, source init.zsh
zinit light starship/starship

# zinit ice depth=1; zinit light romkatv/powerlevel10k
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
# [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

zinit creinstall -q $HOME/.zsh/completion

zinit ice src"util.zsh"; zinit light $HOME/.zsh/util

setopt auto_menu
setopt auto_cd
setopt auto_list
setopt auto_param_keys
setopt auto_param_slash
setopt auto_resume
setopt list_packed
setopt rec_exact
setopt correct
setopt complete_in_word
setopt globdots
setopt interactive_comments
setopt inc_append_history
setopt hist_no_store
setopt HIST_REDUCE_BLANKS
setopt no_beep
setopt nolistbeep
setopt notify
setopt list_types
setopt share_history
setopt list_packed

alias dc=cd
alias md='mkdir'
alias rm='rm -ri'
alias g='git'
alias h='hg'

alias vi='vim'
alias t='tig'
alias ta='tig --all'

case "${OSTYPE}" in
    darwin* )
        alias l='ls -1a -G'
        alias ls="ls -G"
        alias ll="ls -lG"
        alias la="ls -laG"
        alias subl='/Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl'
        ;;
    linux* )
        alias l='ls -1a --color'
        alias ls='ls --color'
        alias ll='ls -l --color'
        alias la='ls -la --color'

        alias blank='sleep 0.2; xset dpms force off'
        alias mutt=neomutt
        ;;
esac

if [ -d ~/.pyenv ]; then
    export PATH="$HOME/.pyenv/bin:$PATH"
    eval "$(pyenv init -)"
fi

if [ -x "`which opam`" ]; then
    eval `opam env`
fi

if [ -f ~/.zshrc.local ]; then
    source ~/.zshrc.local
fi
