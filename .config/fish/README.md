# fishfiles

This is my configuration files for fish shell.

## Install

Install fish shell by Homebrew.

```bash
brew install fish
```

And change the login shell to fish.

Then, execute as below.

```bash
mkdir -p .config/fish
git clone git@github.com:3tty0n/fishfiles.git ~/.config/fish
curl -Lo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
cd ~/.config/fish
```
Next, change your shell to `fish`, restart the sesion and install the plugins.

```zsh
chsh -s /usr/local/bin/fish
fish
cd ~/.config/fish
fisher
```
