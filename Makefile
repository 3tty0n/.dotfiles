build:
	/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
	cd brew
	brew tap Homebrew/bundle
	brew bundle
	ln -s ~/.dotfiles/.vimrc ~/.vimrc
	ln -s ~/.dotfiles/.zshrc ~/.zshrc
	ln -s ~/.dotfiles/.tmux.conf ~/.tmux.conf
	ln -s ~/.dotfiles/.gitconfig ~/.gitconfig
	ln -s ~/.dotfiles/.gitignore_global ~/.gitignore_global
	ln -s ~/.dotfiles/.irbrc ~/.irbrc

build.mini:
	/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
	cd brew
	mv Brewfile _Brewfile
	mv Brewfile-mini Brewfile
	brew tap Homebrew/bundle
	brew bundle
	ln -s ~/.dotfiles/.vimrc ~/.vimrc
	ln -s ~/.dotfiles/.zshrc ~/.zshrc
	ln -s ~/.dotfiles/.tmux.conf ~/.tmux.conf
	ln -s ~/.dotfiles/.gitconfig ~/.gitconfig
	ln -s ~/.dotfiles/.gitignore_global ~/.gitignore_global
	ln -s ~/.dotfiles/.irbrc ~/.irbrc
