#!/usr/bin/env bash

NAME="$@"
maim --select --padding 30 --delay 3 --capturebackground "$HOME/image/$(date +%F-%T)-${NAME}.png"
