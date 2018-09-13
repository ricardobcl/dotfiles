#!/bin/sh

if test ! $(which brew); then
    echo "Installing homebrew"
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# update homebrew
brew update

echo "Installing homebrew packages..."


echo "Install Inconsolata Font for Terminal"
brew install homebrew/cask-fonts/font-inconsolata

# cli tools
brew install wget

# development tools
brew install git
brew install tmux
brew install zsh
brew install antigen # managing zsh plugins
brew install markdown

# improved cli tools from this article:
# https://remysharp.com/2018/08/23/cli-improved

brew install bat # a better cat
brew install fzf # a better ctrl-r for fuzzy search in terminal
# To install useful key bindings and fuzzy completion:
$(brew --prefix)/opt/fzf/install
brew install htop # a better top
brew install diff-so-fancy # a better diff, spefically for 'git diff'
brew install fd # a better find
brew install ncdu # a better du
brew install tldr # a better man
# brew install ack # a better ag and grep
brew install ripgrep # better than ack?
brew install jq # parsing JSON
brew install exa # a better ls
brew install httpie # a better curl/wget

exit 0
