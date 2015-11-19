#!/bin/sh

if test ! $(which brew); then
    echo "Installing homebrew"
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew update

echo "Installing homebrew packages..."

# cli tools
brew install wget


# development tools
brew install git
brew install macvim --override-system-vim
brew install reattach-to-user-namespace
brew install tmux
brew install zsh
brew install highlight
brew install markdown
brew install rlwrap

# install neovim
brew install neovim/neovim/neovim

# installed as of 19-11-2015

# asciidoc                        gdb                             libffi                          protobuf
# autoconf                        gdbm                            libgit2                         py2cairo
# automake                        gdk-pixbuf                      libmpc                          python
# boost                           gettext                         libpng                          r
# boot2docker                     gflags                          librsvg                         rbenv
# brew-cask                       ghc                             libssh2                         rbenv-bundler
# cairo                           git                             libtiff                         readline
# cloog                           glib                            libtool                         reattach-to-user-namespace
# cmake                           glog                            libyaml                         rebar
# coreutils                       gmp                             little-cms2                     rebar3
# cscope                          gnupg                           lua                             rlwrap
# csshx                           gnuplot                         macvim                          ruby
# devil                           gobject-introspection           makedepend                      ruby-build
# docbook                         graphviz                        matplotlib                      rust
# docker                          grc                             maven                           sqlite
# elixir                          harfbuzz                        mpfr                            tmux
# emacs-mac                       htop-osx                        neovim                          unixodbc
# erlang                          icu4c                           netcat                          vim
# exercism                        imagemagick                     node                            wakeonlan
# fftw                            intltool                        numpy                           webp
# fish                            isl                             openssl                         wget
# fontconfig                      jpeg                            pango                           wxmac
# freeglut                        keybase                         pcre                            xz
# freetype                        kjell                           pixman                          zlib
# gcc                             libcroco                        pkg-config
# gd                              libevent                        postgresql

exit 0
