#!/usr/bin/env bash

# This is a utility script for switching the keyboard layout
# TODO: It would be nice to have a variable with languages
#       that this script could use for cycle through them.
#GENTOO_REQ: sys-apps/gawk x11-apps/setxkbmap

current=$(setxkbmap -query | grep layout | awk '{print $2}')

if [ "${current}" == "us" ]; then
    setxkbmap -layout no
elif [ "${current}" == "no" ]; then
    setxkbmap -layout es
else
    setxkbmap -layout us
fi
