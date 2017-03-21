build:
	/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
	cp ./brew/Brewfile .
	brew tap Homebrew/bundle
	brew bundle
	rm Brewfile

oh-my-zsh:
	sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

symlink:
	ln -s ~/.dotfiles/.vimrc ~/.vimrc
	ln -s ~/.dotfiles/.zshrc ~/.zshrc
	ln -s ~/.dotfiles/.tmux.conf ~/.tmux.conf
	ln -s ~/.dotfiles/.gitconfig ~/.gitconfig
	ln -s ~/.dotfiles/.gitignore_global ~/.gitignore_global
	ln -s ~/.dotfiles/.irbrc ~/.irbrc
	ln -s ~/.dotfies/vim/ftplugin ~/.vim/ftplugin
	ln -s ~/.dotfiles/vim/snippets ~/.vim/snippets

build.mini:
	/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
	cp ./brew/Brewfile-mini ./Brewfile
	brew tap Homebrew/bundle
	brew bundle
