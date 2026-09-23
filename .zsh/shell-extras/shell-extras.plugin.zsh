# Local plugin: fuzzy-finder integrations, tool hooks, SDK/version-manager
# init, and machine-local overrides. Sourced via `plugin` from .zshrc.mini.

# ── Fuzzy finders ─────────────────────────────────────────────────────────────
# fzf: Ctrl-R / Ctrl-T / Alt-C
[[ -f $HOME/.zsh/plugins/fzf/shell/key-bindings.zsh ]] &&
  source $HOME/.zsh/plugins/fzf/shell/key-bindings.zsh

# ghq + z fuzzy (Tab / Ctrl-G)
[[ -f ${DOT_ZSH_ROOT:-$HOME/.zsh}/fzf-extensions/init.zsh ]] &&
  source ${DOT_ZSH_ROOT:-$HOME/.zsh}/fzf-extensions/init.zsh

# ── Tool hooks ────────────────────────────────────────────────────────────────
command -v direnv >/dev/null && eval "$(direnv hook zsh)"
command -v opam >/dev/null && eval "$(opam env 2>/dev/null)"
[[ -r $HOME/.opam/opam-init/init.zsh ]] &&
  source $HOME/.opam/opam-init/init.zsh >/dev/null 2>&1

# ── SDKs / version managers ───────────────────────────────────────────────────
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s $SDKMAN_DIR/bin/sdkman-init.sh ]] && source $SDKMAN_DIR/bin/sdkman-init.sh

export NVM_DIR="$HOME/.config/nvm"
[[ -s $NVM_DIR/nvm.sh ]] && source $NVM_DIR/nvm.sh
[[ -s $NVM_DIR/bash_completion ]] && source $NVM_DIR/bash_completion

[[ -d $HOME/.npm-global ]] && export PATH="$HOME/.npm-global/bin:$PATH"
[[ -d $HOME/.opencode ]] && export PATH="$HOME/.opencode/bin:$PATH"

# ── Local / machine-specific ──────────────────────────────────────────────────
[[ -f ${DOT_ZSH_ROOT:-$HOME/.zsh}/claude-go.sh ]] &&
  source ${DOT_ZSH_ROOT:-$HOME/.zsh}/claude-go.sh

[[ -f $HOME/.zshrc.local ]] && source $HOME/.zshrc.local
