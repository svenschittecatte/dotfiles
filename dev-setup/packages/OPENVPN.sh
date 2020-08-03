#!/usr/bin/env bash

set -e

# -------- OPENVPN -------- #
sudo apt install apt-transport-https
wget https://swupdate.openvpn.net/repos/openvpn-repo-pkg-key.pub
sudo apt-key add openvpn-repo-pkg-key.pub
sudo wget -O /etc/apt/sources.list.d/openvpn3.list https://swupdate.openvpn.net/community/openvpn3/repos/openvpn3-focal.list
sudo apt update
sudo apt install -y openvpn3
openvpn3 session-start --config Downloads/fw1-TCP4-1194-sven.schittecatte-config.ovpn
