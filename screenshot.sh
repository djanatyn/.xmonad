#!/usr/bin/env bash

NAME="$@"
maim --select "$HOME/image/$(date +%F-%T)-${NAME}.png"
