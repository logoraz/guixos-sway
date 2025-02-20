#!/bin/sh
# Borrowed from:
# https://codeberg.org/daviwil/dotfiles/.files/.bin/toggle-display
# TODO: needs to be tested in accordance to my config
if [ $# -eq 0 ]; then
    echo "No arguments provided. Please provide the names of the displays to toggle."
    exit 1
fi

for display in "$@"
do
    # Run the swaymsg command with the current display name
    echo "Trying display: $display..."
    swaymsg output $display toggle
done
