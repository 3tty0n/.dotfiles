if [[ -d `ghq root`/github.com/tarjoilija/zgen ]]; then
  source `ghq root`/github.com/tarjoilija/zgen/zgen.zsh
else
  ghq get tarjoilija/zgen
  source `ghq root`/github.com/tarjoilija/zgen/zgen.zsh
fi

export ZGEN_RESET_ON_CHANGE=($HOME/.zsh/zgenrc.zsh)
if ! zgen saved; then
  echo "Creating a zgen save"

  zgen load zsh-users/zsh-history-substring-search
  zgen load zsh-users/zsh-syntax-highlighting
  zgen load zsh-users/zsh-autosuggestions
  zgen load zsh-users/zsh-completions src
  zgen load rupa/z
  zgen load b4b4r07/enhancd
  zgen load mollifier/cd-gitroot
  zgen load changyuheng/zsh-interactive-cd
  zgen load ~/.zsh/util
  zgen save
fi