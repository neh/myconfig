#!/bin/bash

CURRENT_LAYOUT=$(setxkbmap -print | awk -F"+" '/xkb_symbols/ {print $2}')

case $CURRENT_LAYOUT in
    us)
	setxkbmap us -variant dvorak -option ctrl:nocaps
	;;
    "us(dvorak)")
	setxkbmap us -option ctrl:nocaps
	;;
esac
