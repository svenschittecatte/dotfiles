#!/usr/bin/env bash

set -e

# -------- PYTHON -------- #
sudo apt-get install software-properties-common
sudo add-apt-repository ppa:deadsnakes/ppa
sudo apt-get update
sudo apt install -y python3.6
pip3 install python-language-server
