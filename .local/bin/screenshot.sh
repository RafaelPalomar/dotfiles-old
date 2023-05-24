#!/bin/bash
set -euo pipefail

CHOICES="Fullscreen\nWindow\nRegion"
CHOICE=$(echo -e $CHOICES | rofi -no-config -no-lazy-grab -dmenu -p "Screenshot Type" -theme ~/.config/polybar/shapes/scripts/rofi/launcher.rasi)

case $CHOICE in
  Fullscreen)
    FILE=$(mktemp /tmp/screenshot.XXXXXX.png)
    maim --delay=1 $FILE
    ;;
  Window)
    FILE=$(mktemp /tmp/screenshot.XXXXXX.png)
    maim -i $(xdotool selectwindow) $FILE
    ;;
  Region)
    FILE=$(mktemp /tmp/screenshot.XXXXXX.png)
    maim -s $FILE
    ;;
  *)
    exit 1
    ;;
esac

CHOICES="Clipboard\nFile"
CHOICE=$(echo -e $CHOICES | rofi -no-config -no-lazy-grab -dmenu -p "Save to" -theme ~/.config/polybar/shapes/scripts/rofi/launcher.rasi)

case $CHOICE in
  Clipboard)
    xclip -selection clipboard -t image/png -i $FILE
    rm $FILE
    notify-send "Screenshot saved to clipboard"
    ;;
  File)
    SAVE_PATH=$(xdg-user-dir PICTURES)/$(date +%Y%m%d-%H%M%S).png
    mv $FILE $SAVE_PATH
    notify-send "Screenshot saved to $SAVE_PATH"
    ;;
  *)
    rm $FILE
    exit 1
    ;;
esac
