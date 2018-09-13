#!/bin/bash

echo "Installing dotfiles"

source install/link.sh

if [ "$(uname)" == "Darwin" ]; then
    echo "Running on OSX"

    echo "Brewing all the things"
    source install/brew.sh

    echo "Updating OSX settings"
    source install/installosx.sh

fi

echo "Configuring git colors"
source git/gitconfig.sh

echo "Configuring zsh as default shell"
chsh -s $(which zsh)

echo "Downloading iTerm theme"
curl -O https://raw.githubusercontent.com/MartinSeeler/iterm2-material-design/master/material-design-colors.itermcolors

echo "Done."
