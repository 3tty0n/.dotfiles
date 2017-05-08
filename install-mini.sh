/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew tap Homebrew/bundle
brew bundle

function mk_symlink() {
		ln -s -i ~/.dotfiles/.vimrc ~/.vimrc
		ln -s -i ~/.dotfiles/.zshrc ~/.zshrc
		ln -s -i ~/.dotfiles/.tmux.conf ~/.tmux.conf
		ln -s -i ~/.dotfiles/.gitconfig ~/.gitconfig
		ln -s -i ~/.dotfiles/.gitignore_global ~/.gitignore_global
		ln -s- i ~/.dotfiles/.irbrc ~/.irbrc
		ln -s -i ~/.dotfies/vim/ftplugin ~/.vim/ftplugin
		ln -s -i ~/.dotfiles/vim/snippets ~/.vim/snippets
}

git clone git@github.com:powerline/fonts.git
./fonts/install.sh
rm -rf fonts
