#!/bin/bash

# build
docker build -f docker/Dockerfile.oasisui_proxy -t coreoasis/oasisui_proxy .
docker build -f docker/Dockerfile.oasisui_app -t coreoasis/oasisui_app .

# run 
docker network create shiny-net
docker-compose up -d 
