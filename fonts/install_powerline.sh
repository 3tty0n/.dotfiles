i#!/bin/sh

# clone
git clone https://github.com/powerline/fonts.git --depth=1
# install
pushd fonts
./install.sh
# clean-up a bit
popd
rm -rf fonts
