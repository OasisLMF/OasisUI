#!/bin/bash

R_VER=4.0.5
SHARED_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"



docker stop rstudio_oasisui && docker rm rstudio_oasisui 
docker build -f docker/Dockerfile.oasisui_development -t oasisui_dev .

docker run -d \
  -p 127.0.0.1:$(echo $R_VER | sed 's/[.]//g')0:8787 \
  -e DISABLE_AUTH=true \
  -e USERID=$UID \
  -v $SHARED_DIR:/home/rstudio/oasisui \
  -e DISPLAY=$DISPLAY \
  -v /tmp/.X11-unix:/tmp/.X11-unix:ro \
  --name rstudio_oasisui \
  oasisui_dev:latest 


echo "connect using: http://localhost:4050/"
