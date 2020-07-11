#!/usr/bin/env bash

set -e

# -------- TMUX -------- #
sudo apt install -y tmux
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
