#!/bin/bash

set -e 
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
INSTALL_BRANCH='main'

cd $SCRIPT_DIR
VALID_GIT_REPO="$(git rev-parse --is-inside-work-tree 2>/dev/null)"

if [ "$VALID_GIT_REPO" = "true" ]; then
    INSTALL_BRANCH=$(git rev-parse --abbrev-ref HEAD)
fi 

echo "Building OasisUI from '$INSTALL_BRANCH'"

# build
#docker build -f docker/Dockerfile.oasisui_proxy -t coreoasis/oasisui_proxy .
docker build --no-cache --progress=plain -f docker/Dockerfile.oasisui_app --pull --build-arg REF_BRANCH=$INSTALL_BRANCH -t coreoasis/oasisui_app .

# run
docker network create shiny-net
docker-compose up -d
