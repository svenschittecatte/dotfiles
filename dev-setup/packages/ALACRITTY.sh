#!/usr/bin/env bash

set -e

# -------- ALACRITTY -------- #
sudo add-apt-repository ppa:mmstick76/alacritty
sudo apt update
sudo apt install -y alacritty
