#!/bin/sh

brew install cabextract
cd ~/Downloads
mkdir consolas
cd consolas
curl -LO https://sourceforge.net/projects/mscorefonts2/files/cabs/PowerPointViewer.exe
cabextract PowerPointViewer.exe
cabextract ppviewer.cab
open CONSOLA*.TTF
