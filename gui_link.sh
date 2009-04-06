#!/bin/sh

ln -s $HOME/myconfig/xbindkeysrc $HOME/.xbindkeysrc
ln -s $HOME/myconfig/xmodmaprc $HOME/.xmodmaprc
ln -s $HOME/myconfig/Xresources $HOME/.Xresources
mkdir $HOME/.xmonad
ln -s $HOME/myconfig/xmonad/xmonad.hs $HOME/.xmonad/xmonad.hs
