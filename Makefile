build:
	/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
	cp ./brew/Brewfile .
	brew tap Homebrew/bundle
	brew bundle
	rm Brewfile
	ln -s ~/.dotfiles/.vimrc ~/.vimrc
	ln -s ~/.dotfiles/.zshrc ~/.zshrc
	ln -s ~/.dotfiles/.tmux.conf ~/.tmux.conf
	ln -s ~/.dotfiles/.gitconfig ~/.gitconfig
	ln -s ~/.dotfiles/.gitignore_global ~/.gitignore_global
	ln -s ~/.dotfiles/.irbrc ~/.irbrc

build.mini:
	/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
	cp ./brew/Brewfile-mini .
	mv Brewfile-mini Brewfile
	brew tap Homebrew/bundle
	brew bundle
	rm Brewfile
	ln -s ~/.dotfiles/.vimrc ~/.vimrc
	ln -s ~/.dotfiles/.zshrc ~/.zshrc
	ln -s ~/.dotfiles/.tmux.conf ~/.tmux.conf
	ln -s ~/.dotfiles/.gitconfig ~/.gitconfig
	ln -s ~/.dotfiles/.gitignore_global ~/.gitignore_global
	ln -s ~/.dotfiles/.irbrc ~/.irbrc
	
