#!/bin/bash

SHARED_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
R_VER=4.0.5
R_PROJECT_FILE=$SHARED_DIR/'oasisui_dev.Rproj'
R_CONTAINER_NAME='rstudio_oasisui'


# if no project file create a new blank file with default configs
if [ ! -f "$R_PROJECT_FILE" ]; then
cat >> $R_PROJECT_FILE <<EOL
Version: 1.0

RestoreWorkspace: Default
SaveWorkspace: Default
AlwaysSaveHistory: Default

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8

RnwWeave: Sweave
LaTeX: pdfLaTeX

BuildType: Package
PackageUseDevtools: Yes
PackagePath: BFE_RShiny/oasisui
PackageInstallArgs: --no-multiarch --with-keep.source
EOL
fi


docker stop $R_CONTAINER_NAME && docker rm $R_CONTAINER_NAME
docker build -f docker/Dockerfile.oasisui_development -t oasisui_dev .

docker run -d \
  #-p 127.0.0.1:$(echo $R_VER | sed 's/[.]//g')0:8787 \
  -e DISABLE_AUTH=true \
  -e USERID=$UID \
  -v $SHARED_DIR:/home/rstudio/oasisui \
  -e DISPLAY=$DISPLAY \
  -v /tmp/.X11-unix:/tmp/.X11-unix:ro \
  --network="host" \
  --name $R_CONTAINER_NAME \
  oasisui_dev:latest

echo "connect using: http://localhost:8787/"
