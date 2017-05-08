def mk_symlink(dotfiles)
  dotfiles.each do |dotfile|
    File.symlink "~/.dotfiles/#{dotfile}", "~/#{dotfile}"
  end
end

def install_homebrew
  `/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"`
  `brew tap Homebrew/bundle`
  `brew bundle`
end

def install_nonascii_fonts
  `git clone git@github.com:powerline/fonts.git \
  ./fonts/install.sh \
  rm -rf fonts`
end

def main
  dotfiles = ['.zshrc', 'zpreztorc', 'vimrc', '.tmux.conf', 'irbrc']
  mk_symlink(dotfiles)
  install_homebrew
  install_nonascii_fonts
end

main
