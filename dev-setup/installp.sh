#!/bin/bash

GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NOCOLOR='\033[0m'

if [ ! -f "$XDG_CACHE_HOME/dev-setup/$1" ]; then
    sh "$HOME/dev-setup/packages/$1".sh && \
        touch $XDG_CACHE_HOME/dev-setup/$1 && \
        echo "${GREEN}$1 Done! ${BLUE}Created $XDG_CACHE_HOME/dev-setup/$1 ...${NOCOLOR}"
else
    echo -e "${GREEN}$XDG_CACHE_HOME/dev-setup/$1 exists! ${BLUE}Skipping...${NOCOLOR}"
fi
