#!/bin/sh
#
# Homebrew
#
# This installs some of the common dependencies needed (or at least desired)
# using Homebrew.

# Check for Homebrew
if test ! $(which brew)
then
  echo "  Installing Homebrew for you."
  ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)" > /tmp/homebrew-install.log
fi

# Install homebrew packages
brew update
brew tap homebrew/science
brew install grc coreutils rlwrap erlang r
# error installing R -> download https://xquartz.macosforge.org

#Installed as of 16-02-14

#ansible apple-gcc42 aspell at-spi2-atk at-spi2-core atk autoconf automake
#bash cairo cairomm cimg clojure clojurescript cloog cmake coreutils curl d-bus
#dart devil elixir emacs erlang ext4fuse faac ffmpeg fish fontconfig freeglut
#freeimage freetype fuse4x-kext gd gdbm gdk-pixbuf gettext gfortran ghostscript
#git glew glib gmp gnuplot go gobject-introspection grc gtk+ gtk+3 gtk-engines
#harfbuzz icu4c ilmbase intltool isl jbig2dec jpeg jsonpp lame leiningen libffi
#libgpg-error libksba libmpc libpng libsigc++ libsvg libsvg-cairo libtiff
#libtool libxml2 libxslt libyaml little-cms2 lua mpfr ntfs-3g openexr openjpeg
#openssl ossp-uuid osxfuse pango pcre pdflib-lite pixman pkg-config poppler
#postgresql r readline rlwrap ruby smlnj spark sqlite swi-prolog texi2html
#unixodbc wget x264 xvid xz yasm zsh

exit 0
